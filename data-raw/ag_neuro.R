# Dataset: ag_neuro
# Description: Create AG test SDTM dataset for Alzheimer's Disease (neuro studies)

# Load libraries ----

library(tibble)
library(dplyr)
library(stringr)
library(admiral)

# Read input data ----

data("nv_neuro")

# Convert blank to NA ----

nv_neuro <- convert_blanks_to_na(nv_neuro)

ag_neuro <- nv_neuro %>%
  dplyr::mutate(DOMAIN = "AG", AGSEQ = NVSEQ, AGLNKID = NVLNKID, AGSTDTC = NVDTC) %>%
  dplyr::mutate(AGTRT = case_when(
    NVCAT == "FBP" ~ "18F-Florbetapir",
    NVCAT == "FBB" ~ "18F-Florbetaben",
    NVCAT == "FTP" ~ "18F-Flortaucipir"
  )) %>%
  dplyr::mutate(AGCAT = case_when(
    NVCAT == "FBP" ~ "AMYLOID TRACER",
    NVCAT == "FBB" ~ "AMYLOID TRACER",
    NVCAT == "FTP" ~ "TAU TRACER"
  )) %>%
  dplyr::mutate(AGDOSE = case_when(
    NVCAT == "FBP" ~ "370 MBq",
    NVCAT == "FBB" ~ "300 MBq",
    NVCAT == "FTP" ~ "370 MBq"
  )) %>%
  dplyr::mutate(AGDOSEU = "MBq") %>%
  dplyr::mutate(AGROUTE = "Intravenous") %>%
  dplyr::select(
    STUDYID, DOMAIN, USUBJID, AGSEQ, AGTRT, AGCAT,
    AGDOSE, AGDOSEU, AGROUTE, AGLNKID,
    VISITNUM, VISIT, AGSTDTC
  )

# Add labels to variables ----

labels <- list(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  AGSEQ = "Sequence Number",
  AGTRT = "Tracer Name",
  AGCAT = "Tracer Category",
  AGDOSE = "Dose per Adminstration",
  AGDOSEU = "Dose Units",
  AGROUTE = "Route of Administration",
  AGLNKID = "Link to Subject's NV Domain Record",
  VISITNUM = "Visit Number",
  VISIT = "Visit Name",
  AGSTDTC = "Date/Time of Administration"
)

for (var in names(labels)) {
  attr(ag_neuro[[var]], "label") <- labels[[var]]
}

# Label AG dataset ----

attr(ag_neuro, "label") <- "Procedure Agents for Nervous System"

# Save dataset ----

usethis::use_data(ag_neuro, overwrite = TRUE)
