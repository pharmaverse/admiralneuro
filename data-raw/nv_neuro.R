# Dataset: nv_neuro
# Description: Create NV test SDTM dataset for Alzheimer's Disease (neuro studies)

# Load libraries ----

library(tibble)
library(dplyr)
library(stringr)
library(admiral)

# Read input data ----

data("dm_neuro")

# Convert blank to NA ----

dm_neuro <- convert_blanks_to_na(dm_neuro)

# Separate placebo and observation from treatment group to mimic different disease progression ----

placebo_group <- dm_neuro %>%
  dplyr::filter(!is.na(ARMCD) & ARMCD == "Pbo")

treatment_group <- dm_neuro %>%
  dplyr::filter(!is.na(ARMCD) & ARMCD == "Xan_Hi")

observation_group <- dm_neuro %>%
  dplyr::filter(is.na(ARMCD) & ARMNRS == "Observational Study")

# Leverage VS visits at BASELINE, WEEK12 and WEEK26

visit_schedule <- pharmaversesdtm::vs %>%
  dplyr::filter(USUBJID %in% dm_neuro$USUBJID) %>%
  dplyr::filter(VISITNUM %in% c(3.0, 9.0, 13.0)) %>%
  dplyr::rename(NVDTC = VSDTC, NVDY = VSDY) %>%
  dplyr::select(USUBJID, VISITNUM, VISIT, VISITDY, NVDTC, NVDY) %>%
  group_by(USUBJID) %>%
  distinct()

# All USUBJID have BASELINE but not all have visits 9 or 13 data

visit9_usubjid <- visit_schedule %>%
  dplyr::filter(VISITNUM == 9) %>%
  dplyr::select(USUBJID) %>%
  distinct() %>%
  unlist()

visit13_usubjid <- visit_schedule %>%
  dplyr::filter(VISITNUM == 13) %>%
  distinct() %>%
  unlist()

# Create records for one USUBJID ----

create_records_for_one_id <- function(usubjid = "01-701-1015", visitnum = 3,
                                      fbp_suvr_cb = 1.461, fbb_suvr_com = 1.452, ftp_suvr_icbgm = 1.331) {
  tibble(
    STUDYID = rep("CDISCPILOT01", 5),
    DOMAIN = rep("NV", 5),
    USUBJID = rep(usubjid, 5),
    SUBJID = sub(".*-", "", rep(usubjid, 5)),
    NVSEQ = 1:5,
    NVTESTCD = c("FBPVR", "FBPSCB", "FBBSCB", "FTPVR", "FTPSICBGM"),
    NVTEST = c(
      "FBP Qualitative Visual Classification",
      "FBP SUVR Ref Cerebellum",
      "FBB SUVR Ref Cerebellum",
      "FTP Qualitative Visual Classification",
      "FTP SUVR Ref Inf Cerebellar GM"
    ),
    NVCAT = c("FBP", "FBP", "FBB", "FTP", "FTP"),
    NVORRES = c("Positive", as.character(fbp_suvr_cb), as.character(fbb_suvr_com), "Positive", as.character(ftp_suvr_icbgm)),
    NVLOC = c(NA, "NEOCORTICAL COMPOSITE", "NEOCORTICAL COMPOSITE", NA, "NEOCORTICAL COMPOSITE"),
    NVNAM = c(NA, "Whole Cerebellum", "Whole Cerebellum", NA, "Inferior Cerebellar Gray Matter"),
    NVMETHOD = c("FBP VISUAL CLASSIFICATION", "AVID FBP SUVR PIPELINE", "BERKELEY FBB SUVR PIPELINE", "FTP VISUAL CLASSIFICATION", "BERKELEY FTP SUVR PIPELINE"),
    VISITNUM = visitnum
  )
}

# Create records for multiple USUBJIDs ----

create_records_for_multiple_ids <- function(ids, visitnum = 3, suvr_lo = 1.25, suvr_hi = 2.5) {
  # Initialize an empty list to store the datasets
  datasets <- list()

  # Set a seed for reproducibility
  set.seed(2774)

  for (i in seq_along(ids)) {
    # Generate random values for the parameters
    fbp_suvr_cb <- round(runif(1, suvr_lo, suvr_hi), 3) # 3 decimal places for suvr
    fbb_suvr_com <- round(fbp_suvr_cb - runif(1, min = 0.005, max = 0.01), 3) # determine suvr values of fbb and ftp from fbp
    ftp_suvr_icbgm <- round(fbp_suvr_cb - runif(1, min = 0.1, max = 0.13), 3)

    # Create the dataset
    datasets[[i]] <- create_records_for_one_id(
      usubjid = ids[i],
      visitnum = visitnum,
      fbp_suvr_cb = fbp_suvr_cb,
      fbb_suvr_com = fbb_suvr_com,
      ftp_suvr_icbgm = ftp_suvr_icbgm
    )
  }

  dplyr::bind_rows(datasets)
}

# Create dataset for visit 3 (baseline) for all ids from dm_neuro ----

all_visit3_dat <- create_records_for_multiple_ids(ids = dm_neuro$USUBJID, visitnum = 3, )

# Create visit 9 dataset for placebo and observational groups ----

pbo_obs_visit9_dat <- all_visit3_dat %>%
  dplyr::filter(USUBJID %in% c(placebo_group$USUBJID, observation_group$USUBJID)) %>%
  dplyr::filter(NVTESTCD %in% c("FBPSCB", "FBBSCB", "FTPSICBGM")) %>%
  dplyr::mutate(
    VISITNUM = 9,
    NVSEQ = NVSEQ + 5,
    NVORRES = as.character(round(as.numeric(NVORRES) + runif(1, min = 0.1, max = 0.2), 3)),
    NVORRES
  )

# Create visit 9 dataset for treatment group ----

treat_visit9_dat <- all_visit3_dat %>%
  dplyr::filter(USUBJID %in% treatment_group$USUBJID) %>%
  dplyr::filter(NVTESTCD %in% c("FBPSCB", "FBBSCB", "FTPSICBGM")) %>%
  dplyr::mutate(
    VISITNUM = 9,
    NVSEQ = NVSEQ + 5,
    NVORRES = ifelse(NVTESTCD %in% c("FBPSCB", "FBBSCB"),
      as.character(round(as.numeric(NVORRES) - runif(1, min = 0.3, max = 0.8), 3)),
      ifelse(NVTESTCD == "FTPSICBGM",
        as.character(round(as.numeric(NVORRES) - runif(1, min = 0.005, max = 0.01), 3)), NVORRES
      )
    )
  )


# Create visit 13 dataset for placebo and observational groups ----

pbo_obs_visit13_dat <- pbo_obs_visit9_dat %>%
  dplyr::filter(NVTESTCD %in% c("FBPSCB", "FBBSCB", "FTPSICBGM")) %>%
  dplyr::mutate(
    VISITNUM = 13,
    NVSEQ = NVSEQ + 5,
    NVORRES = as.character(round(as.numeric(NVORRES) + runif(1, min = 0.2, max = 0.3), 3)),
    NVORRES
  )

# Create visit 13 dataset for treatment group ----

treat_visit13_dat <- treat_visit9_dat %>%
  # dplyr::filter(USUBJID %in% visit_13_usubjid$USUBJID) %>%
  dplyr::filter(NVTESTCD %in% c("FBPSCB", "FBBSCB", "FTPSICBGM")) %>%
  dplyr::mutate(
    VISITNUM = 13,
    NVSEQ = NVSEQ + 5,
    NVORRES = ifelse(NVTESTCD %in% c("FBPSCB", "FBBSCB"),
      as.character(round(as.numeric(NVORRES) - runif(1, min = 0.3, max = 0.8), 3)),
      ifelse(NVTESTCD == "FTPSICBGM",
        as.character(round(as.numeric(NVORRES) - runif(1, min = 0.01, max = 0.05), 3)), NVORRES
      )
    )
  )

# Combine datasets and add additional variables ----

all_dat <- bind_rows(
  all_visit3_dat,
  pbo_obs_visit9_dat %>%
    dplyr::filter(USUBJID %in% visit9_usubjid),
  treat_visit9_dat %>%
    dplyr::filter(USUBJID %in% visit9_usubjid),
  pbo_obs_visit13_dat %>%
    dplyr::filter(USUBJID %in% visit13_usubjid),
  treat_visit13_dat %>%
    dplyr::filter(USUBJID %in% visit13_usubjid)
) %>%
  dplyr::mutate(
    NVBLFL = ifelse(VISITNUM == 3, "Y", NA_character_),
    NVORRESU = ifelse(NVTESTCD %in% c("FBPSCB", "FBBSCB", "FTPSICBGM"),
      "RATIO", NA
    ),
    NVSTRESC = NVORRES,
    NVSTRESN = ifelse(NVTESTCD %in% c("FBPSCB", "FBBSCB", "FTPSICBGM"),
      suppressWarnings(as.numeric(NVORRES)), NA
    ),
    NVSTRESU = ifelse(NVTESTCD %in% c("FBPSCB", "FBBSCB", "FTPSICBGM"),
      "RATIO", NA
    )
  ) %>%
  dplyr::left_join(
    visit_schedule,
    by = c("USUBJID", "VISITNUM")
  ) %>%
  dplyr::select(
    STUDYID, DOMAIN, USUBJID, NVSEQ,
    NVTESTCD, NVTEST, NVCAT,
    NVLOC, NVMETHOD, NVNAM,
    NVORRES, NVORRESU, NVSTRESC, NVSTRESN, NVSTRESU,
    VISITNUM, VISIT, NVDTC, NVDY,
    NVBLFL
  )

# Add labels to variables ----

labels <- list(
  # Identifier Variables (Key variables)
  STUDYID = "Study Identifier",
  DOMAIN = "Domain Abbreviation",
  USUBJID = "Unique Subject Identifier",
  NVSEQ = "Sequence Number",

  # Topic Variables\
  NVTESTCD = "Nervous System Test Short Name",
  NVTEST = "Nervous System Test Name",
  NVCAT = "Category for Nervous System Test",

  # Qualifier Variables
  NVLOC = "Location of Nervous System Finding",
  NVMETHOD = "Method of Test or Examination",
  NVNAM = "Nervous System Finding Subcategory",

  # Result Variables
  NVORRES = "Result or Finding in Original Units",
  NVORRESU = "Original Units",
  NVSTRESC = "Character Result/Finding in Std Format",
  NVSTRESN = "Numeric Result/Finding in Standard Units",
  NVSTRESU = "Standard Units",

  # Timing Variables
  VISITNUM = "Visit Number",
  VISIT = "Visit Name",
  NVDTC = "Date/Time of Collection",
  NVDY = "Study Day of Collection",

  # Additional Qualifier
  NVBLFL = "Baseline Flag"
)

for (var in names(labels)) {
  attr(all_dat[[var]], "label") <- labels[[var]]
}

nv_neuro <- all_dat

# Label NV dataset ----

attr(nv_neuro, "label") <- "Nervous System Findings"

# Save dataset ----

usethis::use_data(nv_neuro, overwrite = TRUE)
