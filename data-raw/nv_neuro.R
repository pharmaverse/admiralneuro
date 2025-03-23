#' Dataset: nv_neuro
#' Description: Create NV test SDTM dataset for Alzheimer's Disease (Neuro)

# Load libraries ----

library(tibble)
library(dplyr)
library(stringr)
library(admiral)

# Read input data ----

data("dm_neuro")
dm_neuro <- convert_blanks_to_na(dm_neuro)

# use vs visit associated with neuro dataset USUBJID
vs <- pharmaversesdtm::vs
visit_schedule <- vs %>%
  dplyr::filter(USUBJID %in% dm_neuro$USUBJID) %>%
  dplyr::filter(VISITNUM %in% c(3.0, 13.0)) %>%
  dplyr::rename(NVDTC = VSDTC, NVDY = VSDY) %>%
  select(USUBJID, VISITNUM, VISIT, NVDTC, NVDY) %>%
  group_by(USUBJID) %>%
  distinct()

# MHTERM has diagnosis of AD or other diagnoses
#mh <- pharmaversesdtm::mh


create_one_visit_dataset <- function(usubjid = "01-701-1015", visitnum = 3,
                                     amy_suvr_cb = 1.461, amy_suvr_com = 1.452, tau_suvr_icbgm = 1.331) {
  tibble(
    STUDYID = rep("CIDSCPILOT01", 5),
    DOMAIN = rep("NV", 5),
    USUBJID = rep(usubjid, 5),
    NVSEQ = 1:5,
    NVTESTCD = c("AMYVR", "AMYSUVRCB", "AMYSUVRCOM", "TAUVR", "TAUSUVRICBGM"),
    NVTEST = c(
      "Qualitative Visual Classification",
      "SUVR Ref Cerebellum",
      "SUVR Ref Composite",
      "Qualitative Visual Classification",
      "SUVR Ref Inf Cerebellar GM"
    ),
    NVORRES = c("Positive", as.character(amy_suvr_cb), as.character(amy_suvr_com), "Positive", as.character(tau_suvr_icbgm)),
    NVNAM = c("AVID", "BERKELEY", "BERKELEY", "AVID", "BERKELEY"),
    NVMETHOD = c("FBP-PET", "FBP-PET", "FBP-PET", "FTP-PET", "FTP-PET"),
    VISITNUM = visitnum
  )
}

visit3_temp_dat <- create_one_visit_dataset()
visit13_temp_dat <- create_one_visit_dataset(usubjid = "01-701-1016", visitnum = 13,
                                        amy_suvr_cb = 1.500, amy_suvr_com = 1.480, tau_suvr_icbgm = 1.350)

create_multiple_visit_datasets <- function(ids, visitnum = 3) {
  # Initialize an empty list to store the datasets
  datasets <- list()

  # Set a seed for reproducibility
  set.seed(3.14)

  for (i in seq_along(ids)) {
    # Generate random values for the parameters
    amy_suvr_cb <- round(runif(1, 1.25, 2.5), 3)
    amy_suvr_com <- round(amy_suvr_cb - runif(1, min = 0.005, max = 0.01), 3)
    tau_suvr_icbgm <- round(amy_suvr_cb - runif(1, min = 0.1, max = 0.13), 3)

    # Create the dataset
    datasets[[i]] <- create_one_visit_dataset(
      amy_suvr_cb = amy_suvr_cb,
      amy_suvr_com = amy_suvr_com,
      tau_suvr_icbgm = tau_suvr_icbgm,
      usubjid = ids[i],
      visitnum = visitnum
    )
  }

  bind_rows(datasets)
}

# Create synthetic datasets
visit3_dat <- create_multiple_visit_datasets(ids = dm_neuro$USUBJID, visitnum = 3)

pbo_visit13_dat <- visit3_dat %>%
  dplyr::filter(NVTESTCD %in% c("AMYSUVRCB", "AMYSUVRCOM", "TAUSUVRICBGM")) %>%
  mutate(
    VISITNUM = 13,
    NVSEQ = NVSEQ + 5,
    NVORRES = as.character(round(as.numeric(NVORRES) + runif(1, min = 0.005, max = 0.01), 3)),
                     NVORRES)

treat_visit13_dat <- visit3_dat %>%
  dplyr::filter(NVTESTCD %in% c("AMYSUVRCB", "AMYSUVRCOM", "TAUSUVRICBGM")) %>%
  mutate(
    VISITNUM = 13,
    NVSEQ = NVSEQ + 5,
    NVORRES = ifelse(NVTESTCD %in% c("AMYSUVRCB", "AMYSUVRCOM"),
                     as.character(round(as.numeric(NVORRES) - runif(1, min = 0.3, max = 0.5), 3)),
                     ifelse(NVTESTCD == "TAUSUVRICBGM",
                            as.character(round(as.numeric(NVORRES) - runif(1, min = 0.005, max = 0.01), 3)), NVORRES)))


all_dat <- bind_rows(
  visit3_dat,
  pbo_visit13_dat,
  treat_visit13_dat) %>%
  mutate(NVBLFL = ifelse(VISITNUM == 3, "Y", NA_character_),
         NVORRESU = ifelse(NVTESTCD %in% c("AMYSUVRCB", "AMYSUVRCOM", "TAUSUVRICBGM"),
                     "RATIO", NA),
         NVSTRESC = NVORRES,
         NVSTRESN = ifelse(NVTESTCD %in% c("AMYSUVRCB", "AMYSUVRCOM", "TAUSUVRICBGM"),
                           suppressWarnings(as.numeric(NVORRES)), NA),
         NVSTRESU = ifelse(NVTESTCD %in% c("AMYSUVRCB", "AMYSUVRCOM", "TAUSUVRICBGM"),
                     "RATIO", NA)) %>%
  dplyr::left_join(
    visit_schedule,
    by = c("USUBJID", "VISITNUM")
  ) %>%
  dplyr::select(
    STUDYID, DOMAIN, USUBJID, NVSEQ, NVTESTCD, NVTEST,
    NVORRES, NVORRESU, NVSTRESC, NVSTRESN, NVSTRESU,
    NVNAM, NVMETHOD, NVBLFL,
    VISITNUM, VISIT, NVDTC, NVDY)


# Add labels to variables
labels <- list(
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  NVSEQ = "Sequence Number",
  NVTESTCD = "Short Name of Nervous System Test",
  NVTEST = "Name of Nervous System Test",
  NVORRES = "Result or Finding in Original Units",
  NVORRESU = "Original Units",
  NVSTRESC = "Character Result/Finding in Std Format",
  NVSTRESN = "Numeric Result/Finding in Standard Units",
  NVSTRESU = "Standard Units",
  NVNAM = "Laboratory or Vendor Name",
  NVMETHOD = "Method of Test of Examination",
  NVBLFL = "Baseline Flag",
  VISITNUM = "Visit Number",
  VISIT = "Visit Name",
  NVDTC = "Date/Time of Collection",
  NVDY = "Study Day of Collection"
)

for (var in names(labels)) {
  attr(all_dat[[var]], "label") <- labels[[var]]
}

nv_neuro <- all_dat

# Label NV dataset ----
attr(nv_neuro, "label") <- "Nervous System Findings"

# Save dataset ----
usethis::use_data(nv_neuro, overwrite = TRUE)
