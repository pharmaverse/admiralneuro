# Dataset: dm_neuro
# Description: Create DM test SDTM dataset for Alzheimer's Disease (Neuro)

# Load libraries -----
library(dplyr)
library(admiral)

# Read input test data from pharmaversesdtm ----
dm <- pharmaversesdtm::dm

# Convert blank to NA ----
dm <- convert_blanks_to_na(dm)

# Subset to first 15 patients who fall between ages 58 and 79, Placebo and
# Xanomeline High Dose only
dm_neuro <- dm %>%
  dplyr::filter(USUBJID %in% c(
    "01-701-1015", "01-701-1023", "01-701-1028", "01-701-1034",
    "01-701-1146", "01-701-1153", "01-701-1181", "01-701-1234",
    "01-701-1275", "01-701-1302", "01-701-1345", "01-701-1360",
    "01-701-1383", "01-701-1392", "01-701-1440"
  ))

# Label dataset ----
attr(dm_neuro, "label") <- "Demographics"

# Save dataset ----
usethis::use_data(dm_neuro, overwrite = TRUE)
