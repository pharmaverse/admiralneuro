# Name: ADLB
#
# Label: Laboratory Analysis Dataset for Neuroscience
#
# Description: This template focuses on neuroscience specific derivations. For additional content
# commonly found in `ADLB` refer to the `admiral` template by running `admiral::use_ad_template("adlb")`.
#
# Input: adsl, lb_neuro

library(admiral)
library(admiralneuro)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)
options(scipen = 999) # turn off scientific notation

# Define project options/variables ----
# Use the admiral option functionality to store subject key variables in one
# place (note: `subject_keys` defaults to STUDYID and USUBJID)
set_admiral_options(subject_keys = exprs(STUDYID, USUBJID))

# Load source datasets ----
# For illustration purposes read in pharmaversesdtm test data
lb_neuro <- pharmaversesdtm::lb_neuro
admiralneuro_adsl <- admiralneuro::adsl_neuro

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/main/articles/concepts_conventions.html#missing # nolint
lb <- convert_blanks_to_na(lb_neuro)
adsl <- convert_blanks_to_na(admiralneuro_adsl)

# Lookup tables ----

# Assign PARAMCD, PARAM, and PARAMN
param_lookup <- tibble::tribble(
  ~LBTESTCD, ~PARAMCD, ~PARAM, ~PARAMN,
  "PTAU217", "PTAU217", "Lumipulse G pTau 217 Plasma (pg/mL)", 1,
  "AMYLB42", "AMYLB42", "Lumipulse G Beta-Amyloid 1-42-N Plasma (pg/mL)", 2,
  "PTAB42R", "PTAB42R", "Lumipulse G pTau 217/Beta-Amyloid 1-42 Plasma Ratio", 3,
  "ASYNASAA", "ASYNASAA", "Alpha Synuclein Seed Amplification Assay (CSF)", 4
)

# Derivations ----

# Get list of ADSL vars required for derivations
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)

adlb <- lb %>%
  ## Join ADSL with LB (need TRTSDT for ADY derivation) ----
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = get_admiral_option("subject_keys")
  )

adlb <- adlb %>%
  ## Add PARAMCD and PARAM ----
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN),
    by_vars = exprs(LBTESTCD)
  )

# Derive Date/Time and Analysis Day ----
# See the "Derive/Impute Numeric Date/Time and Analysis Day" vignette section
# for more information:
# (https://pharmaverse.github.io/admiral/main/articles/bds_finding.html#datetime)

# Add analysis date (ADT)
adlb <- adlb %>%
  derive_vars_dt(new_vars_prefix = "A", dtc = LBDTC) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))

## Get visit info ----
# See also the "Visit and Period Variables" vignette
# (https://pharmaverse.github.io/admiral/main/articles/visits_periods.html#visits)
# Derive analysis visit (AVISIT, AVISITN)
adlb <- adlb %>%
  mutate(
    AVISIT = case_when(
      !is.na(VISIT) ~ str_to_title(VISIT),
      TRUE ~ NA_character_
    ),
    AVISITN = case_when(
      AVISIT == "Baseline" ~ 0,
      str_detect(VISIT, "WEEK") ~ as.integer(str_extract(VISIT, "\\d+")),
      TRUE ~ NA_integer_
    ),
    BASETYPE = "LAST"
  )

# Derive results ----
# See the "Derive Results (AVAL, AVALC)" vignette section for more information:
# (https://pharmaverse.github.io/admiral/main/articles/bds_finding.html#aval)

# Derive AVAL and AVALC and define parameter categories
adlb <- adlb %>%
  mutate(
    LBSTRESN2 = case_when(
      PARAMN == 1 ~ round(LBSTRESN, 4),
      PARAMN == 2 ~ round(LBSTRESN, 1),
      PARAMN == 3 ~ round(LBSTRESN, 5),
      PARAMN == 4 ~ LBSTRESN,
      TRUE ~ NA
    ),
    AVAL = LBSTRESN,
    # Only populate AVALC if character value is non-redundant with AVAL
    AVALC = if_else(
      is.na(AVAL) | as.character(signif(LBSTRESN2, 5)) != LBSTRESC,
      LBSTRESC,
      NA_character_
    ),
    ANRLO = LBSTNRLO,
    ANRHI = LBSTNRHI
  ) %>%
  select(!LBSTRESN2)

# Derive domain specific parameters ----
# See the "Derive Additional Parameters" vignette section for general
# information about domain specific parameters:
# (https://pharmaverse.github.io/admiral/main/articles/bds_finding.html#derive_param)

# Derive LOG-tranformed AMYLB42 for further analyses and plotting
adlb <- adlb %>%
  derive_param_computed(
    by_vars = exprs(!!!get_admiral_option("subject_keys"), AVISIT, AVISITN, ADT, ADY, !!!adsl_vars),
    parameters = "AMYLB42",
    set_values_to = exprs(
      AVAL = log(AVAL.AMYLB42),
      PARAMCD = "LAMYLB42",
      PARAM = "LOG-Transform Lumipulse G Beta-Amyloid 1-42-N Plasma (pg/mL)",
      PARAMN = 5
    )
  )

## Calculate ONTRTFL ----
adlb <- adlb %>%
  derive_var_ontrtfl(
    start_date = ADT,
    ref_start_date = TRTSDT,
    ref_end_date = TRTEDT,
    filter_pre_timepoint = toupper(AVISIT) == "BASELINE" # Observations as not on-treatment
  )

### Derive Baseline flags ABLFL ----
adlb <- adlb %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ABLFL,
      by_vars = c(get_admiral_option("subject_keys"), exprs(BASETYPE, PARAMCD)),
      order = exprs(ADT, VISITNUM, LBSEQ),
      mode = "last"
    ),
    filter = ((!is.na(AVAL) | !is.na(AVALC)) & ADT <= TRTSDT & !is.na(BASETYPE))
  )

## Derive analysis flags based on visit----

### ANL01FL: Flag last result within a visit and timepoint for baseline and on-treatment post-baseline records ----
adlb <- adlb %>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      new_var = ANL01FL,
      by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, AVISIT)),
      order = exprs(ADT, AVAL),
      mode = "last"
    ),
    filter = !is.na(AVISITN) & (ONTRTFL == "Y" | ABLFL == "Y")
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
    filter = !is.na(AVISITN) & (ONTRTFL == "Y" | ABLFL == "Y")
  )

## Derive baseline information ----

### Calculate BASE ----
adlb <- adlb %>%
  derive_var_base(
    by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, BASETYPE)),
    source_var = AVAL,
    new_var = BASE
  ) %>%
  ### Calculate BASEC ----
  derive_var_base(
    by_vars = c(get_admiral_option("subject_keys"), exprs(PARAMCD, BASETYPE)),
    source_var = AVALC,
    new_var = BASEC
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

## Assign ASEQ (Optional Variable) ----
adlb <- adlb %>%
  derive_var_obs_number(
    new_var = ASEQ,
    by_vars = get_admiral_option("subject_keys"),
    order = exprs(PARAMCD, ADT, AVISITN, VISITNUM),
    check_type = "error"
  )

# Final Steps, Select final variables and Add labels ----
# This process will be based on your metadata, no example given for this reason

# Save output ----

# Change to whichever directory you want to save the dataset in
dir <- tools::R_user_dir("admiralneuro_templates_data", which = "cache")

if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(adlb, file = file.path(dir, "adlb.rda"), compress = "bzip2")
