# Creating ADNV

## Introduction

This article describes the creation of the `ADNV` (Nervous System
Analysis Dataset) ADaM.

The `ADNV` dataset focuses on olfactory function assessments,
specifically the University of Pennsylvania Smell Identification Test
(UPSIT). This vignette demonstrates the neuro-specific steps for
creating this BDS finding dataset.

We advise you first consult the
[admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html).
The programming workflow around creating the general set-up of a BDS
ADaM dataset using [admiral](https://pharmaverse.github.io/admiral/)
functions is the same. In this vignette, we focus on the neuro-specific
steps to avoid repeating information and maintaining the same content in
two places. As such, the code in this vignette is not completely
executable; we recommend consulting the `ADNV` template script to view
the full workflow.

**Note**: *All examples assume CDISC SDTM and/or ADaM format as input
unless otherwise specified.*

### Required Packages

The examples of this vignette require the following packages.

``` r
library(admiral)
library(admiralneuro)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)
library(stringr)
```

## Programming Workflow

- [Read in Data](#readdata)
- [Initial Setup](#adnv_setup)
- [Assign `PARAMCD`, `PARAM`, and `PARAMN`](#paramcd)
- [Map `AVAL` and `AVALC` variables](#aval)
- [Convert UPSIT Score to Percentile](#percentile)
- [Derive Criterion Flag Variables for UPSIT Percentile](#criterionfl)
- [Remaining Analysis Dataset Set-up](#dataset_end)

### Read in Data

To start, all data frames needed for the creation of `ADNV` should be
loaded into the global environment. Reading data will usually be a
company specific process, however, for the purpose of this vignette, we
will use example data from
[pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/) and
[admiralneuro](https://pharmaverse.github.io/admiralneuro/). We will
utilize `NV` and `ADSL` for the basis of the Nervous System Analysis
dataset.

``` r
nv <- pharmaversesdtm::nv_neuro %>%
  convert_blanks_to_na()

adsl <- admiralneuro::adsl_neuro %>%
  convert_blanks_to_na()
```

### Initial Setup

The following steps are used to merge `ADSL` variables with the source
data `NV`, followed with filtering to retain only UPSIT records. Common
analysis variables such as Analysis Date (`ADT`) and Relative Analysis
Day (`ADY`) can also be derived during this process. Note that only the
sections relevant to this vignette are included in the steps below. To
get detailed guidance on all the steps, refer to the
[admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html).

``` r
adsl_vars <- exprs(AGE, SEX, TRTSDT, TRTEDT, TRT01A, TRT01P)

adnv <- nv %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  filter(NVTESTCD == "UPSIT") %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = NVDTC
  ) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))
```

#### Assign `PARAMCD`, `PARAM`, and `PARAMN` variables

The next step is to create and assign parameter level variables such as
`PARAMCD`, `PARAM`, and `PARAMN`. For this, a lookup can be created
based on the SDTM `--TESTCD` and `--CAT` values to join to the source
data.

``` r
param_lookup <- tibble::tribble(
  ~NVTESTCD, ~NVCAT, ~PARAMCD, ~PARAM, ~PARAMN,
  "UPSIT", "OLFACTORY FUNCTION", "UPSITTS", "UPSIT Combined Score from 40 Odorant", 1
)
```

This lookup may now be joined to the data:

``` r
adnv <- adnv %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN),
    by_vars = exprs(NVTESTCD, NVCAT)
  )
```

#### Map `AVAL` and `AVALC` variables

Now that the parameter-level variables have been created, we can map the
Analysis Value variables. For these parameters, no complex derivation is
required. Note that `AVALC` should only be mapped if it contains
non-redundant information.

``` r
adnv <- adnv %>%
  mutate(
    AVAL = NVSTRESN,
    AVALC = if_else(
      is.na(NVSTRESN) | as.character(NVSTRESN) != NVSTRESC,
      NVSTRESC,
      NA
    )
  )
```

We then drop AVALC column because it contains no non-missing values

``` r
if (all(is.na(adnv$AVALC))) {
  adnv <- adnv %>% select(-AVALC)
}
```

### Convert UPSIT Score to Percentile

The **UPSIT (University of Pennsylvania Smell Identification Test)** is
a standardized test that measures olfactory function using 40 odor
identification items. The raw UPSIT score ranges from 0 to 40, with
higher scores indicating better olfactory function.

To enable meaningful interpretation across different age and sex groups,
raw UPSIT scores can be converted to **percentiles** based on normative
data. These percentiles represent where an individual’s performance
ranks relative to age- and sex-matched peers.

In the following steps, we will incorporate the **conversion** from
**UPSIT total score to percentile** using the function
[`admiralneuro::compute_upsit_percentile()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_upsit_percentile.md).

**1. Define the Source Variables**

The first step is to define `keep_vars`, which lists the source
variables to retain and copy into newly derived records in the next
step; all other variables will remain in the dataset but will be set to
`NA` in those derived rows.

**2. Convert UPSIT Score to Percentile**

The second step creates new analysis records in which the original
`UPSITTS` values are converted into percentiles using
[`admiralneuro::compute_upsit_percentile()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_upsit_percentile.md).
The conversion is based on the patient’s sex and age at the time of
assessment.

The new parameter values which include `AVAL`, `PARAMCD`, `PARAM`, and
`PARAMN` are assigned for the percentile parameter.

``` r
keep_vars <- c(
  get_admiral_option("subject_keys"),
  adsl_vars,
  exprs(ADT, ADY, VISIT)
)

# Prepare derived rows
upsit_percentile <- adnv %>%
  filter(PARAMCD == "UPSITTS") %>%
  mutate(
    AVAL = compute_upsit_percentile(
      sex = SEX,
      age = AGE,
      upsit_score = AVAL
    ),
    PARAMCD = "UPSITPC",
    PARAM = "Percentile derived from UPSIT total score",
    PARAMN = 2,
    AVALU = NA
  )

# Bind with original dataset
adnv <- bind_rows(adnv, upsit_percentile)

# Drop AVALU because it contains no non-missing values
if (all(is.na(adnv$AVALU))) {
  adnv <- adnv %>% select(-AVALU)
}
```

This is how the parameters and newly converted parameters will look
like:

#### Derive Criterion Flag Variables for UPSIT Percentile

Having converted the UPSIT score to percentile, we can now derive the
criterion flags for olfactory impairment categories.

We will use a percentile cutoff of **10** to define olfactory
impairment. The 10th percentile cutoff represents a sex- and
age-adjusted threshold for olfactory impairment based on normative data.
UPSIT percentile ≤ 10 is considered olfactory impairment, while UPSIT
percentile \> 10 is considered normal olfactory function.

To do this, we will use the
[`admiral::derive_vars_crit_flag()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/derive_vars_crit_flag.html)
function. Since we want to derive these flags specifically for the UPSIT
percentile parameter, we will apply the derivation only to those records
using
[`admiral::restrict_derivation()`](https:/pharmaverse.github.io/admiral/v1.4.0/cran-release/reference/restrict_derivation.html).

``` r
adnv <- adnv %>%
  restrict_derivation(
    derivation = derive_vars_crit_flag,
    args = params(
      crit_nr = 1,
      condition = if_else(PARAMCD == "UPSITPC", AVAL <= 10, NA),
      description = "UPSITPC <= 10",
      values_yn = TRUE # To get "Y", "N", and NA for the flag
    ),
    filter = PARAMCD == "UPSITPC"
  )
```

This is how the criterion flags will look like:

### Remaining Analysis Dataset Set-up

The [admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html)
covers all the steps that are not shown here, such as merging the timing
variables, analysis flags, baseline derivations, change from baseline
calculations, etc.

## Example Scripts

| ADaM | Sourcing Command                                             |
|------|--------------------------------------------------------------|
| ADNV | `admiral::use_ad_template("ADNV", package = "admiralneuro")` |

### References

University of Pennsylvania Smell Identification Test. (n.d.). In
*Wikipedia*. Retrieved from
<https://en.wikipedia.org/wiki/University_of_Pennsylvania_Smell_Identification_Test>

Brumm, M. C. *et al.* (2023). Updated Percentiles for the University of
Pennsylvania Smell Identification Test in Adults 50 Years of Age and
Older. *Neurology, 100*(16), e1691–e1701.

Mastenbroek, S. E. *et al.* (2025). Two-step detection of Lewy body
pathology via smell-function testing and CSF α-synuclein seed
amplification. *Nature Communications, 16*, 7182.
