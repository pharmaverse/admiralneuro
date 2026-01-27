# Compute Centiloid Value

Computes the Centiloid value based on an amyloid Positron Emission
Tomography (PET) scan radioactive tracer, Standardized Uptake Value
Ratio (SUVR) value, pipeline, and reference region. Also allows for
custom formula parameters.

## Usage

``` r
compute_centiloid(
  tracer,
  pipeline,
  ref_region,
  suvr,
  custom_slope = NULL,
  custom_intercept = NULL
)
```

## Arguments

- tracer:

  Amyloid PET tracer

  A character string is expected. If `custom_slope` and
  `custom_intercept` are specified, this parameter is ignored. See
  Details section for accepted values in combination with `pipeline` and
  `ref_region`.

  Permitted values

  :   a character scalar, i.e., a character vector of length one

  Default value

  :   none

- pipeline:

  SUVR pipeline

  A character string is expected. If `custom_slope` and
  `custom_intercept` are specified, this parameter is ignored. See
  Details section for accepted values in combination with `tracer` and
  `ref_region`.

  Permitted values

  :   a character scalar, i.e., a character vector of length one

  Default value

  :   none

- ref_region:

  Reference region

  A character string is expected. If `custom_slope` and
  `custom_intercept` are specified, this parameter is ignored. See
  Details section for accepted values in combination with `tracer` and
  `ref_region`.

  Permitted values

  :   a character scalar, i.e., a character vector of length one

  Default value

  :   none

- suvr:

  SUVR value

  A numeric value is expected.

  Permitted values

  :   a numeric scalar, i.e., a numeric vector of length one

  Default value

  :   none

- custom_slope:

  Optional slope parameter for custom Centiloid calculation formula

  A numeric value is expected when provided. When `custom_slope` is
  specified (along with `custom_intercept`), this overrides the standard
  formula parameters `tracer`, `pipeline`, and `ref_region`. Default is
  `NULL`.

  Permitted values

  :   a numeric scalar, i.e., a numeric vector of length one

  Default value

  :   `NULL`

- custom_intercept:

  Optional intercept parameter for custom centiloid calculation formula

  A numeric value is expected when provided. When `custom_intercept` is
  specified (along with `custom_slope`), this overrides the standard
  formula parameters `tracer`, `pipeline`, and `ref_region`. Default is
  `NULL`.

  Permitted values

  :   a numeric scalar, i.e., a numeric vector of length one

  Default value

  :   `NULL`

## Value

A numeric Centiloid value.

## Details

The Centiloid scale is a standardized quantitative measure for amyloid
PET imaging that allows comparison between different tracers and
analysis methods. This function converts SUVR values to the Centiloid
scale based on published conversion equations for specific tracer,
pipeline, and reference region combinations.

Centiloid is calculated as:

\$\$Centiloid = slope \times SUVR + intercept\$\$

where slope and intercept are formula parameters. If `custom_slope` and
`custom_intercept` are not specified, this function uses pre-defined
slope and intercept based on the user's selections of tracer, pipeline,
and reference region.

The combinations of tracer, pipeline and reference region in the table
below are supported. The columns "slope" and "intercept" then show the
values of the slope and intercept that `compute_centiloid()` will use to
calculate the centiloid value in each case.

|                 |                             |                  |        |           |
|-----------------|-----------------------------|------------------|--------|-----------|
| tracer          | pipeline                    | ref_region       | slope  | intercept |
| 18F-Florbetapir | AVID FBP SUVR PIPELINE¹     | Whole Cerebellum | 183.07 | -177.26   |
| 18F-Florbetaben | AVID FBB SUVR PIPELINE²     | Whole Cerebellum | 156.06 | -148.13   |
| 18F-Florbetapir | BERKELEY FBP SUVR PIPELINE³ | Whole Cerebellum | 188.22 | -189.16   |
| 18F-Florbetaben | BERKELEY FBB SUVR PIPELINE³ | Whole Cerebellum | 157.15 | -151.87   |

The equations used for the conversions are based on the following
references:

¹ Navitsky, et al. (2018).
[doi:10.1016/j.jalz.2018.06.1353](https://doi.org/10.1016/j.jalz.2018.06.1353)
² Sims, et al. (2024).
[doi:10.1001/jama.2023.13239](https://doi.org/10.1001/jama.2023.13239) ³
Royse, et al. (2021).
[doi:10.1186/s13195-021-00836-1](https://doi.org/10.1186/s13195-021-00836-1)

Alternatively, the user can override the pre-selection by specifying
both `custom_slope` and `custom_intercept` instead. When `custom_slope`
and `custom_intercept` are specified, the function ignores `tracer`,
`pipeline` and `ref_region` for calculation purposes. However, this
function **always requires** specification of `tracer`, `pipeline`, and
`ref_region` parameters, even when using custom slope and intercept
values. This design choice ensures that users remain cognizant of the
imaging context and analysis methodology when computing Centiloid
values.

For additional Centiloid transformation formulas, see: Iaccarino, L. et
al. (2025).
[doi:10.1016/j.nicl.2025.103765](https://doi.org/10.1016/j.nicl.2025.103765)

If a matching combination of tracer, pipeline, and reference region is
not specified and both `custom_slope` and `custom_intercept` are not
specified, the function aborts with an error.

## See also

Other com_bds_findings:
[`compute_upsit_percentile()`](https://pharmaverse.github.io/admiralneuro/dev/reference/compute_upsit_percentile.md)

## Examples

### Using standard parameters

Computes Centiloid with predefined slope/intercept for supported
tracer/pipeline/ref_region combinations:

    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    )
    #> [1] 51.5775

### Using custom parameters

Computes Centiloid by overriding slope/intercept using custom values:

    compute_centiloid(
      tracer = "MyTracer",
      pipeline = "MyPipeline",
      ref_region = "MyRegion",
      suvr = 1.25,
      custom_slope = 193,
      custom_intercept = -187
    )
    #> [1] 54.25
