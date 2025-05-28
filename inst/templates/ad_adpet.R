# Name: ADPET
#
# Label: PET Scan Analysis Dataset
#
# Input: adsl, nv,

library(admiral)
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
# For illustration purposes read in admiral test data
nv <- admiralneuro::nv_neuro
ag <- admiralneuro::ag_neuro
suppnv <- admiralneuro::suppnv_neuro
adsl <- admiralneuro::adsl_neuro

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint
nv <- convert_blanks_to_na(nv)


# Combine the parental datasets with their respective supp datasets (only if exist)
# User can use `combine_supp()` from {metatools} to combine the parental with supp dataset.

nv <- metatools::combine_supp(nv, suppnv)

# Lookup tables ----

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~NVTESTCD, ~NVCAT, ~NVLOC, ~REFREG, ~PARAMCD, ~PARAM, ~PARAMN,
  "SUVR", "FBP", "NEOCORTICAL COMPOSITE", "Whole Cerebellum", "SNCWCFBP", "FBP Standard Uptake Ratio Neocortical Composite Whole Cerebellum", 1,
  "SUVR", "FBB", "NEOCORTICAL COMPOSITE", "Whole Cerebellum", "SNCWCFBB", "FBB Standard Uptake Ratio Neocortical Composite Whole Cerebellum", 2,
  "SUVR", "FTP", "NEOCORTICAL COMPOSITE", "Inferior Cerebellar Gray Matter", "SNCTFTP", "FTP Standard Uptake Ratio Neocortical Composite Inferior Cerebellar Gray Matter", 3,
  "VR", "FBP", NA, NA, "VRFBP", "FBP Qualitative Visual Classification", 4,
  "VR", "FTP", NA, NA, "VRFTP", "FTP Qualitative Visual Classification", 5
)
attr(param_lookup$NVTESTCD, "label") <- "NV Test Short Name"

# Derivations ----

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)

adpet <- nv %>%
  # Join ADSL with NV (need TRTSDT for ADY derivation)
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  ) %>%
  # Join ADPET with AG for tracer information, users can add more variables in the `new_vars` argument as needed.
  derive_vars_merged(
    dataset_add = ag,
    new_vars = exprs(AGTRT, AGCAT),
    by_vars = exprs(USUBJID, VISIT, NVLNKID = AGLNKID)
  ) %>%
  ## Calculate ADT, ADY ----
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = NVDTC
  ) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

adpet <- adpet %>%
  ## Add PARAMCD only - add PARAM etc later ----
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM),
    by_vars = exprs(NVTESTCD, NVCAT, NVLOC)
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

## Get visit info ----
# See also the "Visit and Period Variables" vignette
# (https://pharmaverse.github.io/admiral/articles/visits_periods.html#visits)
adpet <- adpet %>%
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
adpet <- adpet %>% derive_var_ontrtfl(
  start_date = ADT,
  ref_start_date = TRTSDT,
  ref_end_date = TRTEDT
)

# Derive Baseline flags

# Calculate ABLFL
adpet <- adpet %>% restrict_derivation(
  derivation = derive_var_extreme_flag,
  args = params(
    new_var = ABLFL,
    by_vars = c(get_admiral_option("subject_keys"), exprs(BASETYPE, PARAMCD)),
    order = exprs(ADT, VISITNUM, NVSEQ),
    mode = "last"
  ),
  filter = ((!is.na(AVAL) | !is.na(AVALC)) & ADT <= TRTSDT & !is.na(BASETYPE))
)

# Derive visit flags

### ANL01FL: Flag last result within a visit and timepoint for baseline and post-baseline records ----
adpet <- adpet %>% restrict_derivation(
  derivation = derive_var_extreme_flag,
  args = params(
    new_var = ANL01FL,
    by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, AVISIT)),
    order = exprs(ADT, AVAL),
    mode = "last"
  ),
  filter = !is.na(AVISITN) & (ONTRTFL == "Y" | ABLFL == "Y")
) %>%
  #### ANL02FL: Flag last result within a PARAMCD for baseline & post-baseline records ----
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ANL02FL,
      by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, ABLFL)),
      order = exprs(ADT),
      mode = "last"
    ),
    filter = !is.na(AVISITN) & (ONTRTFL == "Y" | ABLFL == "Y")
  )

# Derive baseline information

# Calculate BASE
adpet <- adpet %>% derive_var_base(
  by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, BASETYPE)),
  source_var = AVAL,
  new_var = BASE
) %>%
  # Calculate BASEC
  derive_var_base(
    by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, BASETYPE)),
    source_var = AVALC,
    new_var = BASEC
  ) %>%
  # Calculate CHG for post-baseline records
  # The decision on how to populate pre-baseline and baseline values of CHG is left to producer choice
  restrict_derivation(
    derivation = derive_var_chg,
    filter = AVISITN > 0
  ) %>%
  # Calculate PCHG for post-baseline records
  # The decision on how to populate pre-baseline and baseline values of PCHG is left to producer choice
  restrict_derivation(
    derivation = derive_var_pchg,
    filter = AVISITN > 0
  )

# Assign ASEQ
adpet <- adpet %>% derive_var_obs_number(
  new_var = ASEQ,
  by_vars = get_admiral_option("subject_keys"),
  order = exprs(PARAMCD, ADT, AVISITN, VISITNUM),
  check_type = "error"
)


# Final Steps, Select final variables and Add labels
# This process will be based on your metadata, no example given for this reason
# ...

admiralneuro_adpet <- adpet

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiraltemplate_templates_data", which = "cache")
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(admiralneuro_adpet, file = file.path(dir, "admiralneuro_adpet.rda"), compress = "bzip2")
