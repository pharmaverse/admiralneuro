# Name: ADNV
#
# Label: Nervous System Analysis Dataset
#
# Input: adsl, nv

library(admiral)
library(admiralneuro)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)

# Define project options/variables ----
# Use the admiral option functionality to store subject key variables in one
# place (note: `subject_keys` defaults to STUDYID and USUBJID)
set_admiral_options(subject_keys = exprs(STUDYID, USUBJID))

# Load source datasets ----

# Use e.g. `haven::read_sas()` to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in pharmaversesdtm and admiralneuro neuro test data
nv <- pharmaversesdtm::nv_neuro
adsl <- admiralneuro::adsl_neuro

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint
nv <- convert_blanks_to_na(nv)
adsl <- convert_blanks_to_na(adsl)

# Lookup tables ----

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~NVTESTCD, ~NVCAT, ~PARAMCD, ~PARAM, ~PARAMN,
  "UPSIT", "OLFACTORY FUNCTION", "UPSITTS", "UPSIT Combined Score from 40 Odorant", 1
)
attr(param_lookup$NVTESTCD, "label") <- "NV Test Short Name"

# Derivations ----

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(AGE, SEX, TRTSDT, TRTEDT, TRT01A, TRT01P)

adnv <- nv %>%
  ## Join ADSL with NV (need TRTSDT for ADY derivation) ----
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  filter(NVTESTCD == "UPSIT") %>% # Filter nv dataset for upsit records only
  ## Calculate ADT, ADY ----
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = NVDTC
  ) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

adnv <- adnv %>%
  ## Add PARAMCD, PARAM and PARAMN ----
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN),
    by_vars = exprs(NVTESTCD, NVCAT)
  ) %>%
  ## Calculate AVAL and AVALC ----
  # AVALC should only be mapped if it contains non-redundant information.
  mutate(
    AVAL = NVSTRESN,
    AVALC = if_else(
      is.na(NVSTRESN) | as.character(NVSTRESN) != NVSTRESC,
      NVSTRESC,
      NA
    )
  )

# Drop AVALC if it contains no non-missing values
if (all(is.na(adnv$AVALC))) {
  adnv <- adnv %>% select(-AVALC)
}

## upsit percentile derivations ----
### Convert UPSITTS to UPSITPC----
keep_vars <- c(
  get_admiral_option("subject_keys"),
  adsl_vars,
  exprs(ADT, ADY, VISIT)
)

# prepare derived rows
upsit_percentile <- adnv %>%
  filter(PARAMCD == "UPSITTS") %>%
  mutate(
    AVAL = compute_upsit_percentile(
      sex = SEX,
      age = AGE,
      upsit_score = AVAL
    ),
    PARAMCD = "UPSITPC",
    PARAM = "Percentile derived from UPSIT total score",
    PARAMN = 2,
    AVALU = NA
  )

# Bind with original dataset
adnv <- bind_rows(adnv, upsit_percentile)

# Drop AVALU if it contains no non-missing values
if (all(is.na(adnv$AVALU))) {
  adnv <- adnv %>% select(-AVALU)
}

# The 10 percentile cutoff represents sex and age adjusted threshold for olfactory impairment
# based on PPMI study
adnv <- adnv %>%
  ### Derive criterion flags for UPSITPC threshold ----
  restrict_derivation(
    derivation = derive_vars_crit_flag,
    args = params(
      crit_nr = 1,
      condition = if_else(PARAMCD == "UPSITPC", AVAL <= 10, NA),
      description = "UPSITPC <= 10",
      values_yn = TRUE # To get "Y", "N", and NA for the flag
    ),
    filter = PARAMCD == "UPSITPC"
  )

## Get visit info ----
# See also the "Visit and Period Variables" vignette
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html#visits)
adnv <- adnv %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN|UNSCHED|RETRIEVAL|AMBUL") ~ NA_character_,
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", "")),
      TRUE ~ NA_character_
    )),
    BASETYPE = "LAST"
  )

## Calculate ONTRTFL ----
adnv <- adnv %>%
  derive_var_ontrtfl(
    start_date = ADT,
    ref_start_date = TRTSDT,
    ref_end_date = TRTEDT,
    filter_pre_timepoint = toupper(AVISIT) == "BASELINE" # Observations as not on-treatment
  )

# Drop ONTRTFL if it contains no non-missing values
if (all(is.na(adnv$ONTRTFL))) {
  adnv <- adnv %>% select(-ONTRTFL)
}

## Derive Baseline flags ----

### Calculate ABLFL ----
adnv <- adnv %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ABLFL,
      by_vars = c(get_admiral_option("subject_keys"), exprs(BASETYPE, PARAMCD)),
      order = exprs(ADT, VISITNUM, NVSEQ),
      mode = "last"
    ),
    filter = (!is.na(AVAL) & ADT <= TRTSDT & !is.na(BASETYPE))
  )

## Derive visit flags ----

### ANL01FL: Flag last result within a visit and timepoint for baseline and on-treatment post-baseline records ----
adnv <- adnv %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ANL01FL,
      by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, AVISIT)),
      order = exprs(ADT, AVAL),
      mode = "last"
    ),
    filter = !is.na(AVISITN) & ABLFL == "Y"
  ) %>%
  ### ANL02FL: Flag last result within a PARAMCD for baseline & on-treatment post-baseline records ----
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ANL02FL,
      by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, ABLFL)),
      order = exprs(ADT),
      mode = "last"
    ),
    filter = !is.na(AVISITN) & ABLFL == "Y"
  )

## Derive baseline information ----

### Calculate BASE ----
adnv <- adnv %>%
  derive_var_base(
    by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, BASETYPE)),
    source_var = AVAL,
    new_var = BASE
  ) %>%
  ### Calculate CHG for post-baseline records ----
  # The decision on how to populate pre-baseline and baseline values of CHG is left as a user choice
  restrict_derivation(
    derivation = derive_var_chg,
    filter = AVISITN > 0
  ) %>%
  ### Calculate PCHG for post-baseline records ----
  # The decision on how to populate pre-baseline and baseline values of PCHG is left to producer choice
  restrict_derivation(
    derivation = derive_var_pchg,
    filter = AVISITN > 0
  )

# Drop CHG if it contains no non-missing values
if (all(is.na(adnv$CHG))) {
  adnv <- adnv %>% select(-CHG)
}

# Drop PCHG if it contains no non-missing values
if (all(is.na(adnv$PCHG))) {
  adnv <- adnv %>% select(-PCHG)
}

## Assign ASEQ ----
adnv <- adnv %>%
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD, ADT, AVISITN, VISITNUM),
    check_type = "error"
  )

# Final Steps, Select final variables and Add labels ----
# This process will be based on your metadata, no example given for this reason

admiralneuro_adnv <- adnv

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralneuro_templates_data", which = "cache")

if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(admiralneuro_adnv, file = file.path(dir, "adnv.rda"), compress = "bzip2")
