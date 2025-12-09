# Dataset: upsit_lookup
# Description: Prepare UPSIT lookup table for compute_upsit_percentile function

library(readxl)
library(dplyr)
library(tidyr)

# Read the Excel file from data-raw directory
upsit_lookup <- read_excel("data-raw/wnl_2023_02_20_brumm_1_sdc1.xlsx",
  sheet = "PercentileTable"
)

# Clean and transform the data
upsit_lookup <- upsit_lookup %>%
  # Convert sex from numeric to character labels
  mutate(sex = case_when(
    sex == 1 ~ "M",
    sex == 2 ~ "F",
    TRUE ~ as.character(sex) # Keep as-is if neither 1 nor 2
  )) %>%
  # Process AgeCat
  mutate(AgeCat_clean = gsub("\\+", "", AgeCat)) %>% # Remove + sign first
  separate(AgeCat_clean,
    into = c("age_low", "age_high"),
    sep = "-",
    fill = "right", # Fill missing values on the right
    convert = TRUE, # Convert to numeric
    remove = TRUE
  ) %>% # Remove the temporary column
  select(-starts_with("AgeCat_clean")) # Clean up if any remnants

labels <- list(
  sex = "Sex",
  AgeCat = "Age category",
  upsit = "UPSIT total raw score",
  age_low = "Lower bound of age category",
  age_high = "Higher bound of age category"
)

for (var in names(labels)) {
  attr(upsit_lookup[[var]], "label") <- labels[[var]]
}

# Label dataset ----
attr(upsit_lookup, "label") <- "UPSIT Lookup Table"

# Save as internal data
usethis::use_data(upsit_lookup, internal = TRUE, overwrite = TRUE)
