# Name: ADSL Neuro
#
# Label: Subject Level Analysis Dataset for Neuro
#
# Input: dm
library(admiral)
library(pharmaversesdtm) # Contains example datasets from the CDISC pilot project
library(dplyr)
library(lubridate)
library(stringr)

# Load source datasets ----

# Use e.g. `haven::read_sas()` to read in .sas7bdat, or other suitable functions
# as needed and assign to the variables below.
# For illustration purposes read in admiral test data

data(dm_neuro)

# When SAS datasets are imported into R using haven::read_sas(), missing
# character values from SAS appear as "" characters in R, instead of appearing
# as NA values. Further details can be obtained via the following link:
# https://pharmaverse.github.io/admiral/articles/admiral.html#handling-of-missing-values # nolint

dm_neuro <- convert_blanks_to_na(dm_neuro)

# Derive Treatment Variables ----
adsl <- dm_neuro %>%
  # Select all necessary variables only
  select(
    STUDYID, USUBJID, SUBJID, SITEID, COUNTRY, AGE, AGEU, SEX, RACE, ETHNIC, ARM,
    ARMCD, ACTARM, ACTARMCD, ARMNRS, DTHDTC, DTHFL, RFXSTDTC, RFXENDTC
    ) %>%
  mutate(
    TRT01P = if_else(!is.na(ARMNRS), "No Treatment", ARM),
    TRT01PN = case_when(
      TRT01P == "No Treatment" ~ 3,
      TRT01P == "Placebo" ~ 2,
      TRUE ~ 1
    ),
    TRT01A = if_else(!is.na(ARMNRS), "No Treatment", ACTARM),
    TRT01AN = case_when(
      TRT01A == "No Treatment" ~ 3,
      TRT01A == "Placebo" ~ 2,
      TRUE ~ 1
    ),
  ) %>%
  select(-ARMNRS)

# Treatment Start and End Date
adsl <- adsl %>%
  derive_vars_dtm(
    dtc = RFXSTDTC,
    new_vars_prefix = "TRTS"
  ) %>%
  derive_vars_dtm(
    dtc = RFXENDTC,
    new_vars_prefix = "TRTE",
    time_imputation = "last"
  ) %>%
  # Convert Datetime variables to date
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%
  # Treatment Start Time
  derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM)) %>%
  # Treatment Duration
  derive_var_trtdurd() %>%
  select(-c(RFXSTDTC, RFXENDTC))

# Derive Age Grouping ----
agegr1_lookup <- exprs(
  ~condition,            ~AGEGR1, ~AGEGR1N,
  is.na(AGE),          "Missing",        3,
  AGE < 65,                "<65",        1,
  !is.na(AGE),            ">=65",        2
)

adsl <- derive_vars_cat(
  dataset = adsl,
  definition = agegr1_lookup
)

# Derive Intent-to-Treat and Safety Population Flags ----
adsl <- adsl %>%
  mutate(ITTFL = "Y") %>%
  derive_var_merged_exist_flag(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    condition = (!is.na(TRTSDT))
  )

# Derive DTHDT & DTHADY ----
adsl <- adsl %>%
  derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC,
    highest_imputation = "M",
    date_imputation = "first"
  ) %>%
  derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  )

admiralneuro_adsl <- adsl

# Final Steps, Select final variables and Add labels
# This process will be based on your metadata, no example given for this reason
# ...

# Save output ----

dir <- tools::R_user_dir("admiralneuro_templates_data", which = "cache")
# Change to whichever directory you want to save the dataset in
if (!file.exists(dir)) {
  # Create the folder
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}
save(admiralneuro_adsl, file = file.path(dir, "adsl.rda"), compress = "bzip2")
