# Creating ADTPET and ADAPET

## Introduction

This article describes the creation of PET Scan ADaMs.

We recommend splitting out amyloid and tau into two distinct ADaM
datasets. The vignette presents a combined approach for the initial
setup, parameter mapping and finals steps. It then splits into separate
sections for each data type, as they require different processes:
starting with `ADTPET` (Tau PET Scan Analysis Dataset), followed by
`ADAPET` (Amyloid PET Scan Analysis Dataset).

We advise you first consult the
[admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html).
The programming workflow around creating the general set-up of a BDS
ADaM dataset using [admiral](https://pharmaverse.github.io/admiral/)
functions is the same. In this vignette, we focus on the neuro-specific
steps to avoid repeating information and maintaining the same content in
two places. As such, the code in this vignette is not completely
executable; we recommend consulting the `ADTPET` and `ADAPET` template
scripts to view the full workflow.

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
library(metatools)
```

## Programming Workflow

- [Read in Data](#readdata)
- [Initial PET Scan Analysis Dataset Set-up](#adpet_setup)
- [Assign `PARAMCD`, `PARAM`, and `PARAMN`](#paramcd)
- [Map `AVAL` and `AVALC` variables](#aval)
- [`ADTPET` (Tau PET Scan Analysis Dataset)](#adtpet)
- [`ADAPET` (Amyloid PET Scan Analysis Dataset)](#adapet)
  - [Convert amyloid SUVR to Centiloid](#centiloid)
  - [Derive Criterion Flag Variables for Amyloid
    Categories](#criterionfl)
- [Remaining PET Scan Analysis Dataset Set-up](#dataset_end)

### Read in Data

To start, all data frames needed for the creation of `ADTPET` or
`ADAPET` should be loaded into the global environment. Reading data will
usually be a company specific process, however, for the purpose of this
vignette, we will use example data from
[pharmaversesdtm](https://pharmaverse.github.io/pharmaversesdtm/) and
[admiralneuro](https://pharmaverse.github.io/admiralneuro/). We will
utilize `NV`, `SUPPNV`, `AG` and `ADSL` for the basis of the PET Scan
ADaM datasets.

``` r
nv <- pharmaversesdtm::nv_neuro %>%
  convert_blanks_to_na()

suppnv <- pharmaversesdtm::suppnv_neuro %>%
  convert_blanks_to_na()

ag <- pharmaversesdtm::ag_neuro %>%
  convert_blanks_to_na()

adsl <- admiralneuro::adsl_neuro %>%
  convert_blanks_to_na()
```

While loading our input data, we can combine the `NV` domain and the
`SUPPNV` supplementary domain for easier use in the next steps. Using
the
[`metatools::combine_supp()`](https://pharmaverse.github.io/metatools/reference/combine_supp.html)
function avoids the need to manually transpose and merge the
supplementary dataset with the corresponding domain.

``` r
nv_suppnv <- combine_supp(nv, suppnv)
```

### Initial PET Scan Analysis Dataset Set-up

The following steps are used to merge `ADSL` variables with the source
data `NV` (previously combined with `SUPPNV`), along with tracer
information from the `AG` dataset, without distinguishing between Tau
and Amyloid data at this stage. This distinction will be made later.
Common analysis variables such as Analysis Date (`ADT`) and Relative
Analysis Day (`ADY`) can also be derived during this process. Note that
only the sections relevant to this vignette are included in the steps
below. To get a detailed guidance on all the steps, refer the
[admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html).

``` r
adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)

adpet <- nv_suppnv %>%
  derive_vars_merged(
    dataset_add = adsl,
    new_vars = adsl_vars,
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  derive_vars_merged(
    dataset_add = ag,
    new_vars = exprs(AGTRT, AGCAT),
    by_vars = exprs(STUDYID, USUBJID, VISIT, NVLNKID = AGLNKID)
  ) %>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = NVDTC
  ) %>%
  derive_vars_dy(reference_date = TRTSDT, source_vars = exprs(ADT))
```

#### Create `PARAMCD`, `PARAM`, and `PARAMN` variables

The next step is to create and assign parameter level variables such as
`PARAMCD`, `PARAM`, and `PARAMN`. For this, a lookup can be created
based on the SDTM `--TESTCD`, `--CAT`, `--LOC`, `--METHOD` and `REFREG`
values to join to the source data.

``` r
param_lookup <- tibble::tribble(
  ~NVTESTCD, ~NVCAT, ~NVLOC, ~REFREG, ~NVMETHOD, ~PARAMCD, ~PARAM, ~PARAMN,
  "SUVR", "FBP", "NEOCORTICAL COMPOSITE", "Whole Cerebellum", "AVID FBP SUVR PIPELINE", "SUVRAFBP", "AVID FBP Standard Uptake Ratio Neocortical Composite Whole Cerebellum", 1,
  "SUVR", "FBB", "NEOCORTICAL COMPOSITE", "Whole Cerebellum", "AVID FBB SUVR PIPELINE", "SUVRAFBB", "AVID FBB Standard Uptake Ratio Neocortical Composite Whole Cerebellum", 2,
  "SUVR", "FBP", "NEOCORTICAL COMPOSITE", "Whole Cerebellum", "BERKELEY FBP SUVR PIPELINE", "SUVRBFBP", "BERKELEY FBP Standard Uptake Ratio Neocortical Composite Whole Cerebellum", 3,
  "SUVR", "FBB", "NEOCORTICAL COMPOSITE", "Whole Cerebellum", "BERKELEY FBB SUVR PIPELINE", "SUVRBFBB", "BERKELEY FBB Standard Uptake Ratio Neocortical Composite Whole Cerebellum", 4,
  "SUVR", "FTP", "NEOCORTICAL COMPOSITE", "Inferior Cerebellar Gray Matter", "AVID FTP SUVR PIPELINE", "SUVRAFTP", "AVID FTP Standard Uptake Ratio Neocortical Composite Inferior Cerebellar Gray Matter", 5,
  "SUVR", "FTP", "NEOCORTICAL COMPOSITE", "Inferior Cerebellar Gray Matter", "BERKELEY FTP SUVR PIPELINE", "SUVRBFTP", "BERKELEY FTP Standard Uptake Ratio Neocortical Composite Inferior Cerebellar Gray Matter", 6,
  "VR", "FBP", NA, NA, "FBP VISUAL CLASSIFICATION", "VRFBP", "FBP Qualitative Visual Classification", 7,
  "VR", "FBB", NA, NA, "FBB VISUAL CLASSIFICATION", "VRFBB", "FBB Qualitative Visual Classification", 8,
  "VR", "FTP", NA, NA, "FTP VISUAL CLASSIFICATION", "VRFTP", "FTP Qualitative Visual Classification", 9
)
```

This lookup may now be joined to the data and this is how the parameters
will look like:

``` r
adpet <- adpet %>%
  derive_vars_merged_lookup(
    dataset_add = param_lookup,
    new_vars = exprs(PARAMCD, PARAM, PARAMN),
    by_vars = exprs(NVTESTCD, NVCAT, NVLOC, NVMETHOD, REFREG)
  )
```

#### Map `AVAL` and `AVALC` variables

Now that the parameter-level variables have been created, we can map the
Analysis Value variables. For these parameters, no complex derivation is
required. Note that `AVALC` should only be mapped if it contains
non-redundant information.

``` r
adpet <- adpet %>%
  mutate(
    AVAL = NVSTRESN,
    AVALC = if_else(
      is.na(NVSTRESN) | as.character(NVSTRESN) != NVSTRESC,
      NVSTRESC,
      NA
    )
  )
```

Having established the foundation of a global PET Scan Analysis Dataset
(`adpet`), we can now differentiate between tau and amyloid data.

### `ADTPET` (Tau PET Scan Analysis Dataset)

Tau PET scan data generally do not require any further derivations, and
the conversion of tau SUVR to CenTauR will be available in the next
release (0.2.0) of this package. Therefore, if producing an `ADTPET`, we
can now filter the global PET Scan Analysis Dataset `adpet` to retain
only the tau tracer records.

``` r
adtpet <- adpet %>%
  filter(AGCAT == "TAU TRACER")
```

### `ADAPET` (Amyloid PET Scan Analysis Dataset)

If producing an `ADAPET`, we can now filter the global PET Scan Analysis
Dataset `adpet` to retain only the amyloid tracer records.

``` r
adapet <- adpet %>%
  filter(AGCAT == "AMYLOID TRACER")
```

#### Convert amyloid SUVR to Centiloid

`SUVR` (Standardized Uptake Value Ratio) is a widely used
semi-quantitative measure in PET imaging that estimates the level of
radiopharmaceutical tracer uptake (such as amyloid) in the brain,
relative to a reference region. While useful, `SUVR` has certain
limitations (which we will not detail here) that affect its
comparability across different tracers and studies.

To address this, the **Centiloid (CL)** scale was developed to
standardize amyloid PET measurements. The scale is anchored such that:

1.  0 CL represents amyloid-negative individuals (usually younger,
    amyloid-negative individual).

2.  100 CL corresponds to the amyloid burden observed in patients with
    Alzheimer’s disease.

This standardization enables consistent interpretation of amyloid burden
across imaging protocols, tracers, and clinical studies.

In the following steps, we will incorporate the **conversion** from
**amyloid SUVR to Centiloid** using the function
[`admiralneuro::compute_centiloid()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_centiloid.md).

For our examples, we divide the process into two main steps:

**1. Define the Source Variables**

The first step is to define `keep_vars`, which lists the source
variables to retain and copy into newly derived records in the next
step; all other variables will remain in the dataset but will be set to
`NA` in those derived rows.

**2. Convert SUVR to Centiloid**

The second step uses
[`admiral::derive_extreme_records()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_extreme_records.html)
to add new analysis records in which the original `SUVR` values are
converted into Centiloid using
[`admiralneuro::compute_centiloid()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_centiloid.md).
The formula is based on tracer, pipeline and reference region; therefore
we apply filtering conditions using the argument `filter_add`, as
follows:

1.  Based on the (`SUVRBFBB`) tracer, the (`BERKELEY FBB SUVR PIPELINE`)
    method, and the (`Whole Cerebellum`) reference region.

2.  Based on the (`SUVRBFBP`) tracer, the (`BERKELEY FBP SUVR PIPELINE`)
    method, and the (`Whole Cerebellum`) reference region.

3.  Based on the (`SUVRAFBB`) tracer, the (`AVID FBB SUVR PIPELINE`)
    method, and the same reference region (`Whole Cerebellum`).

4.  Based on the (`SUVRAFBP`) tracer, the (`AVID FBP SUVR PIPELINE`)
    method, and the same reference region (`Whole Cerebellum`).

The new parameter values which include `AVAL`, `PARAMCD`, `PARAM`, and
`AVALU`, are assigned within the `set_values_to` argument of this
function.

In this step, centiloid values are derived independently for each row
that meets the specified filtering condition, with separate derivations
performed for the four defined filtering conditions.

``` r
keep_vars <- c(
  get_admiral_option("subject_keys"),
  adsl_vars,
  exprs(ADT, ADY, VISIT)
)

adapet <- adapet %>%
  derive_extreme_records(
    dataset = .,
    dataset_add = .,
    filter_add = (PARAMCD == "SUVRBFBB" & NVMETHOD == "BERKELEY FBB SUVR PIPELINE" & REFREG == "Whole Cerebellum"),
    set_values_to = exprs(
      AVAL = compute_centiloid(
        tracer = "18F-Florbetaben",
        pipeline = "BERKELEY FBB SUVR PIPELINE",
        ref_region = "Whole Cerebellum",
        suvr = AVAL
      ),
      PARAMCD = "CENTLD",
      PARAM = "Centiloid value derived from SUVR pipeline",
      AVALU = "CL"
    ),
    keep_source_vars = exprs(!!!keep_vars)
  ) %>%
  derive_extreme_records(
    dataset = .,
    dataset_add = .,
    filter_add = (PARAMCD == "SUVRBFBP" & NVMETHOD == "BERKELEY FBP SUVR PIPELINE" & REFREG == "Whole Cerebellum"),
    set_values_to = exprs(
      AVAL = compute_centiloid(
        tracer = "18F-Florbetapir",
        pipeline = "BERKELEY FBP SUVR PIPELINE",
        ref_region = "Whole Cerebellum",
        suvr = AVAL
      ),
      PARAMCD = "CENTLD",
      PARAM = "Centiloid value derived from SUVR pipeline",
      AVALU = "CL"
    ),
    keep_source_vars = exprs(!!!keep_vars)
  ) %>%
  derive_extreme_records(
    dataset = .,
    dataset_add = .,
    filter_add = (PARAMCD == "SUVRAFBB" & NVMETHOD == "AVID FBB SUVR PIPELINE" & REFREG == "Whole Cerebellum"),
    set_values_to = exprs(
      AVAL = compute_centiloid(
        tracer = "18F-Florbetaben",
        pipeline = "AVID FBB SUVR PIPELINE",
        ref_region = "Whole Cerebellum",
        suvr = AVAL
      ),
      PARAMCD = "CENTLD",
      PARAM = "Centiloid value derived from SUVR pipeline",
      AVALU = "CL"
    ),
    keep_source_vars = exprs(!!!keep_vars)
  ) %>%
  derive_extreme_records(
    dataset = .,
    dataset_add = .,
    filter_add = (PARAMCD == "SUVRAFBP" & NVMETHOD == "AVID FBP SUVR PIPELINE" & REFREG == "Whole Cerebellum"),
    set_values_to = exprs(
      AVAL = compute_centiloid(
        tracer = "18F-Florbetapir",
        pipeline = "AVID FBP SUVR PIPELINE",
        ref_region = "Whole Cerebellum",
        suvr = AVAL
      ),
      PARAMCD = "CENTLD",
      PARAM = "Centiloid value derived from SUVR pipeline",
      AVALU = "CL"
    ),
    keep_source_vars = exprs(!!!keep_vars)
  )
```

This is how the parameters and newly converted parameters will look
like:

#### Derive Criterion Flag Variables for Amyloid Categories

Having converted the `SUVR` to Centiloid, we can now derive the
criterion flags for the amyloid categories.

We will use Centiloid cutoff of **24.1** to define amyloid negative and
positive categories. The 24.1 Centiloid cutoff comes from an
autopsy-validated study in which amyloid PET SUVR values were compared
with post-mortem brain amyloid plaque counts (Clark *et al.*, 2012). The
SUVR threshold corresponding to moderate-to-frequent neuritic plaques
under the Consortium to Establish a Registry for Alzheimer’s Disease
(CERAD) criteria was mapped to the Centiloid scale, yielding 24.1 CL as
the pathology-based definition of amyloid positivity (Navitsky *et al.*,
2018). Centiloid \>= 24.1 is considered amyloid positive, while
Centiloid \< 24.1 is considered amyloid negative.

To do this, we will use the
[`admiral::derive_vars_crit_flag()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/derive_vars_crit_flag.html)
function. Since we want to derive these flags specifically for the
Centiloid parameters, we will apply the derivation only to those records
using
[`admiral::restrict_derivation()`](https:/pharmaverse.github.io/admiral/v1.4.1/cran-release/reference/restrict_derivation.html).

``` r
adapet <- adapet %>%
  restrict_derivation(
    derivation = derive_vars_crit_flag,
    args = params(
      crit_nr = 1,
      condition = if_else(PARAMCD == "CENTLD", AVAL < 24.1, NA),
      description = "CENTILOID < 24.1",
      values_yn = TRUE # To get "Y", "N", and NA for the flag
    ),
    filter = PARAMCD == "CENTLD"
  )
```

This is how the criterion flags will look like:

### Remaining PET Scan Analysis Dataset Set-up

The [admiral](https://pharmaverse.github.io/admiral/) [Creating a BDS
Finding ADaM
vignette](https://pharmaverse.github.io/admiral/articles/bds_finding.html)
covers all the steps that are not shown here, such as merging the timing
variables, analysis flags, etc.

## Example Scripts

| ADaM   | Sourcing Command                                               |
|--------|----------------------------------------------------------------|
| ADTPET | `admiral::use_ad_template("ADTPET", package = "admiralneuro")` |
| ADAPET | `admiral::use_ad_template("ADAPET", package = "admiralneuro")` |

### References

Clark, C. M. *et al.* (2012). Cerebral PET with florbetapir compared
with neuropathology at autopsy for detection of neuritic amyloid-β
plaques: a prospective cohort study. *The Lancet Neurology, 11*(8),
669–678.

Navitsky, M. *et al.* (2018). Standardization of amyloid quantitation
with florbetapir standardized uptake value ratios to the Centiloid
scale. *Alzheimer’s & Dementia, 14*(12), 1570–1577.
