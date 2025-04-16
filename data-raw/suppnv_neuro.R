# Dataset: suppnv_neuro
# Description: Create SUPPNV test SDTM dataset for Alzheimer's Disease (neuro studies)

# Load libraries ----

library(tibble)
library(dplyr)
library(stringr)
library(admiral)

# Read input data ----

data("nv_neuro")

# Convert blank to NA ----

nv_neuro <- convert_blanks_to_na(nv_neuro)

suppnv_neuro <- nv_neuro %>%
  dplyr::filter(!is.na(NVLOC)) %>%
  dplyr::mutate(RDOMAIN = "NV", IDVAR = "NVSEQ", IDVARVAL = NVSEQ, QNAM = "REFREG", QLABEL = "Reference Region") %>%
  dplyr::mutate(QVAL = case_when(
    stringr::str_detect(NVTEST, "Ref Cerebellum|FBB SUVR Ref Cerebellum") ~ "Whole Cerebellum",
    stringr::str_detect(NVTEST, "Ref Inf Cerebellar GM") ~ "Inferior Cerebellar Gray Matter",
    TRUE ~ NA_character_)) %>%
  dplyr::select(STUDYID, RDOMAIN, USUBJID, IDVAR, IDVARVAL, QNAM, QLABEL, QVAL)

# Add labels to variables ----

labels <- list(
  STUDYID = "Study Identifier",
  RDOMAIN = "Related Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  IDVAR = "Identifying Variable",
  IDVARVAL = "Identifying Variable Value",
  QNAM = "Qualifier Variable Name",
  QLABEL = "Qualifier Variable Label",
  QVAL = "Qualifier Value")

for (var in names(labels)) {
  attr(suppnv_neuro[[var]], "label") <- labels[[var]]
}

# Label NV dataset ----

attr(suppnv_neuro, "label") <- "Supplemental to Nervous System Findings"

# Save dataset ----

usethis::use_data(suppnv_neuro, overwrite = TRUE)
