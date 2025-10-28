# Dataset: adsl_neuro
# Description: Create ADSL test Analysis Dataset for Alzheimer's Disease (Neuro)

#' @importFrom admiral convert_blanks_to_na derive_vars_dtm derive_vars_dtm_to_dt
#' derive_vars_dtm_to_tm derive_var_trtdurd derive_vars_cat derive_var_merged_exist_flag
#' derive_vars_duration
#' @importFrom dplyr filter mutate select pull if_else case_when left_join
#' @importFrom lubridate days
#' @importFrom usethis use_data
#' @noRd

# Read input test data from pharmaversesdtm ----
data(dm_neuro)
ex <- pharmaversesdtm::ex

# Convert blank to NA ----
dm_neuro <- admiral::convert_blanks_to_na(dm_neuro)
ex <- admiral::convert_blanks_to_na(ex)

# Select patients from DM Neuro only
dm_neuro_pat <- dm_neuro %>%
  dplyr::pull(unique(USUBJID))

ex <- ex %>%
  dplyr::filter(USUBJID %in% dm_neuro_pat)

# Derive Treatment Variables ----
adsl <- dm_neuro %>%
  # Select all necessary variables only
  dplyr::select(
    STUDYID, USUBJID, SUBJID, SITEID, COUNTRY, AGE, AGEU, SEX, RACE, ETHNIC, ARM,
    ARMCD, ACTARM, ACTARMCD, ARMNRS, DTHDTC, DTHFL
  ) %>%
  dplyr::mutate(
    TRT01P = dplyr::if_else(!is.na(ARMNRS), "No Treatment", ARM),
    TRT01PN = dplyr::case_when(
      TRT01P == "No Treatment" ~ 3,
      TRT01P == "Placebo" ~ 2,
      TRUE ~ 1
    ),
    TRT01A = dplyr::if_else(!is.na(ARMNRS), "No Treatment", ACTARM),
    TRT01AN = dplyr::case_when(
      TRT01A == "No Treatment" ~ 3,
      TRT01A == "Placebo" ~ 2,
      TRUE ~ 1
    )
  )

# Treatment Start and End Dates ----
ex_ext <- ex %>%
  admiral::derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  admiral::derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    time_imputation = "last"
  ) %>%
  # Merge DM.ARMNRS to not derive treatment dates for these patients
  dplyr::left_join(dm_neuro %>% dplyr::select(USUBJID, ARMNRS), by = "USUBJID")

adsl <- adsl %>%
  # Treatment Start Datetime
  admiral::derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (is.na(ARMNRS)),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Treatment End Datetime
  admiral::derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (is.na(ARMNRS)),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  # Convert Datetime variables to date
  admiral::derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%
  # Treatment Start Time
  admiral::derive_vars_dtm_to_tm(source_vars = exprs(TRTSDTM)) %>%
  # Treatment Duration
  admiral::derive_var_trtdurd()

# Derive Age Grouping ----
agegr1_lookup <- exprs(
  ~condition,   ~AGEGR1, ~AGEGR1N,
  is.na(AGE), "Missing",        3,
  AGE < 65,       "<65",        1,
  !is.na(AGE),   ">=65",        2
)

adsl <- derive_vars_cat(
  dataset = adsl,
  definition = agegr1_lookup
)

# Derive Intent-To-Treat and Safety Population Flags ----
adsl <- adsl %>%
  dplyr::mutate(ITTFL = "Y") %>%
  admiral::derive_var_merged_exist_flag(
    dataset_add = adsl,
    by_vars = exprs(STUDYID, USUBJID),
    new_var = SAFFL,
    false_value = "N",
    condition = (!is.na(TRTSDT))
  )

# Derive DTHDT & DTHADY ----
adsl <- adsl %>%
  admiral::derive_vars_dt(
    new_vars_prefix = "DTH",
    dtc = DTHDTC,
    highest_imputation = "M",
    date_imputation = "first"
  ) %>%
  admiral::derive_vars_duration(
    new_var = DTHADY,
    start_date = TRTSDT,
    end_date = DTHDT
  )

adsl_neuro <- adsl

# Assign the labels for created variables
labels <- list(
  DTHDTC = "Date/Time of Death",
  DTHFL = "Subject Death Flag",
  TRT01P = "Planned Treatment for Period 01",
  TRT01PN = "Planned Treatment for Period 01 (N)",
  TRT01A = "Actual Treatment for Period 01",
  TRT01AN = "Actual Treatment for Period 01 (N)",
  TRTSDTM = "Datetime of First Exposure to Treatment",
  TRTSTMF = "Time 1st Exposure Period Imput. Flag",
  TRTEDTM = "Datetime of Last Exposure to Treatment",
  TRTETMF = "Time Last Exposure Period Imput. Flag",
  TRTSDT = "Date of First Exposure to Treatment",
  TRTEDT = "Date of Last Exposure to Treatment",
  TRTSTM = "Time of First Exposure to Treatment",
  TRTDURD = "Treatment Duration",
  AGEGR1 = "Pooled Age Group 1",
  AGEGR1N = "Pooled Age Group 1 (N)",
  ITTFL = "Intent-To-Treat Population Flag",
  SAFFL = "Safety Population Flag",
  DTHDT = "Date of Death",
  DTHDTF = "Date of Death Imputation Flag",
  DTHADY = "Relative Day of Death"
)

for (var in names(labels)) {
  attr(adsl_neuro[[var]], "label") <- labels[[var]]
}

# Label dataset ----
attr(adsl_neuro, "label") <- "Subject-Level Analysis Dataset"

# Save dataset ----
usethis::use_data(adsl_neuro, overwrite = TRUE)
