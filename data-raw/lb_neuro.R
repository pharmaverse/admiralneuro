# Dataset: lb_neuro
# Description: Create LB test SDTM dataset for Alzheimer's Disease (Neuro)

# Load libraries
library(tibble)
library(dplyr)
library(admiral)
# Read input data
data("dm_neuro")
data("nv_neuro")
data("adsl_neuro")
ex <- pharmaversesdtm::ex

# Convert blank to NA
dm_neuro <- convert_blanks_to_na(dm_neuro)
ex <- convert_blanks_to_na(ex)

# Select patients from DM Neuro only (following adsl_neuro.R pattern)
dm_neuro_pat <- dm_neuro %>%
  pull(USUBJID) %>%
  unique()

ex <- ex %>%
  filter(USUBJID %in% dm_neuro_pat)

# Get visit schedule from NV data for all visits
visit_schedule <- nv_neuro %>%
  dplyr::filter(VISIT == "BASELINE") %>%
  dplyr::select(USUBJID, VISITNUM, VISIT, NVDTC, NVDY) %>%
  dplyr::group_by(USUBJID, VISIT) %>%
  dplyr::arrange(as.Date(NVDTC)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::rename(LBDTC = NVDTC, LBDY = NVDY) %>%
  distinct()

# Derive treatment start date to calculate VISITDY (following adsl_neuro.R pattern)
ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST"
  ) %>%
  # Merge DM.ARMNRS to not derive treatment dates for these patients
  left_join(dm_neuro %>% select(USUBJID, ARMNRS), by = "USUBJID")

# Get treatment start dates per subject
trtsdt_lookup <- ex_ext %>%
  filter(is.na(ARMNRS)) %>%
  group_by(USUBJID) %>%
  arrange(EXSTDTM, EXSEQ) %>%
  slice(1) %>%
  ungroup() %>%
  derive_vars_dtm_to_dt(source_vars = exprs(EXSTDTM)) %>%
  select(USUBJID, TRTSDT = EXSTDT)

# Add VISITDY calculation to visit schedule
visit_schedule <- visit_schedule %>%
  left_join(trtsdt_lookup, by = "USUBJID") %>%
  mutate(
    VISITDY = case_when(
      !is.na(TRTSDT) ~ as.numeric(as.Date(LBDTC) - TRTSDT + 1),
      TRUE ~ NA_real_
    )
  ) %>%
  select(-TRTSDT)

# Get subject characteristics with treatment information from adsl_neuro ----
subject_chars <- dm_neuro %>%
  dplyr::select(STUDYID, USUBJID) %>%
  dplyr::left_join(
    adsl_neuro %>% dplyr::select(STUDYID, USUBJID, TRT01P, TRT01PN, TRTSDT),
    by = c("STUDYID", "USUBJID")
  ) %>%
  dplyr::mutate(
    # Create treatment groups for analysis
    TRTGRP = case_when(
      TRT01P %in% c("Placebo", "No Treatment") ~ "PLACEBO_CONTROL",
      TRUE ~ "TREATMENT"
    )
  )

# Create LB record for one subject-visit ----
create_lb_record <- function(usubjid, visitnum, visit, visitdy, lbdtc, lbdy, trtgrp) {
  # Define positive rates based on treatment group and visit
  pos_rate <- case_when(
    trtgrp == "PLACEBO_CONTROL" ~ 0.30, # 30% positive for placebo/control at all visits
    trtgrp == "TREATMENT" & visit == "BASELINE" ~ 0.30, # Assume baseline similar for all
    TRUE ~ 0.00 # Default
  )

  # Generate SAA result based on positive rate
  lborres <- if (runif(1) < pos_rate) "Positive" else "Negative"

  tibble(
    STUDYID = "CDISCPILOT01",
    DOMAIN = "LB",
    USUBJID = usubjid,
    LBSEQ = "missing",
    LBTESTCD = "ASYNASAA",
    LBTEST = "Alpha-synuclein Seed Amplification Assay (CSF)",
    LBCAT = "Biomarkers",
    LBORRES = lborres,
    LBORRESU = NA_character_,
    LBORNRLO = NA_character_,
    LBORNRHI = NA_character_,
    LBSTRESC = lborres,
    LBSTRESN = case_when(
      lborres == "Positive" ~ 1,
      lborres == "Negative" ~ 0,
      TRUE ~ NA_real_
    ),
    LBSTRESU = NA_character_,
    LBSTNRLO = NA_real_,
    LBSTNRHI = NA_real_,
    LBNRIND = NA_character_,
    LBBLFL = ifelse(visit == "BASELINE", "Y", NA_character_),
    VISITNUM = visitnum,
    VISIT = visit,
    VISITDY = visitdy,
    LBDTC = lbdtc,
    LBDY = lbdy
  )
}

# Set seed for reproducibility
set.seed(2774)

# Generate all LB records
all_lb_records <- visit_schedule %>%
  dplyr::left_join(subject_chars, by = c("USUBJID")) %>%
  dplyr::rowwise() %>%
  dplyr::do(
    create_lb_record(
      usubjid = .$USUBJID,
      visitnum = .$VISITNUM,
      visit = .$VISIT,
      visitdy = .$VISITDY,
      lbdtc = .$LBDTC,
      lbdy = .$LBDY,
      trtgrp = .$TRTGRP
    )
  ) %>%
  dplyr::ungroup()

# Add sequence numbers and finalize
lb_neuro <- all_lb_records %>%
  dplyr::arrange(USUBJID, VISITNUM) %>%
  dplyr::group_by(USUBJID) %>%
  dplyr::mutate(LBSEQ = row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    STUDYID, DOMAIN, USUBJID, LBSEQ, LBTESTCD, LBTEST, LBCAT,
    LBORRES, LBORRESU, LBORNRLO, LBORNRHI, LBSTRESC, LBSTRESN,
    LBSTRESU, LBSTNRLO, LBSTNRHI, LBNRIND, LBBLFL,
    VISITNUM, VISIT, VISITDY, LBDTC, LBDY
  )

# Create subject-level SAA flags for ADSL
saa_flags <- lb_neuro %>%
  dplyr::group_by(STUDYID, USUBJID) %>%
  dplyr::summarize(
    SAAFL = ifelse(any(LBSTRESC == "Positive"), "Y", "N"),
    SAAPOSDT = suppressWarnings(min(as.Date(LBDTC[LBSTRESC == "Positive"]), na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    SAAPOSDT = ifelse(is.infinite(SAAPOSDT), NA, SAAPOSDT),
    SAAPOSDT = as.character(SAAPOSDT)
  )

# Validation checks
expected_visits <- c("BASELINE", "WEEK 12", "WEEK 26")
stopifnot(all(lb_neuro$VISIT %in% expected_visits))
stopifnot(all(lb_neuro$LBORRES %in% c("Positive", "Negative")))
stopifnot(all(nchar(lb_neuro$LBTESTCD) <= 8))

# Check for one record per subject per visit
visit_counts <- lb_neuro %>%
  dplyr::count(USUBJID, VISIT) %>%
  dplyr::filter(n > 1)

if (nrow(visit_counts) > 0) {
  warning("Duplicate records found for some subjects/visits")
}

# Assign labels for lb_neuro variables
labels <- list(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  LBSEQ = "Sequence Number",
  LBTESTCD = "Lab Test or Examination Short Name",
  LBTEST = "Lab Test or Examination Name",
  LBCAT = "Category for Lab Test",
  LBORRES = "Result or Finding in Original Units",
  LBORRESU = "Original Units",
  LBORNRLO = "Reference Range Lower Limit in Orig Unit",
  LBORNRHI = "Reference Range Upper Limit in Orig Unit",
  LBSTRESC = "Character Result/Finding in Std Format",
  LBSTRESN = "Numeric Result/Finding in Standard Units",
  LBSTRESU = "Standard Units",
  LBSTNRLO = "Reference Range Lower Limit-Std Units",
  LBSTNRHI = "Reference Range Upper Limit-Std Units",
  LBNRIND = "Reference Range Indicator",
  LBBLFL = "Baseline Flag",
  VISITNUM = "Visit Number",
  VISIT = "Visit Name",
  VISITDY = "Planned Study Day of Visit",
  LBDTC = "Date/Time of Specimen Collection",
  LBDY = "Study Day of Specimen Collection"
)

for (var in names(labels)) {
  attr(lb_neuro[[var]], "label") <- labels[[var]]
}

# Label dataset
attr(lb_neuro, "label") <- "Laboratory Test Results Dataset"

# Save dataset
usethis::use_data(lb_neuro, overwrite = TRUE)
