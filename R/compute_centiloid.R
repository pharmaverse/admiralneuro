#' Compute Centiloid Value
#'
#' Computes centiloid values based on amyloid PET tracer, SUVR value and pipeline,
#' and reference region.
#' Also allows for custom formula parameters.
#'
#' @param tracer Amyloid PET tracer
#'
#'   A character string is expected.
#'   If `custom_slope` and `custom_intercept` are specified, this parameter is ignored.
#'   See Details section for accepted values in combination with `pipeline` and `ref_region`.
#'
#' @param pipeline SUVR pipeline
#'
#'   A character string is expected.
#'   If `custom_slope` and `custom_intercept` are specified, this parameter is ignored.
#'   See Details section for accepted values in combination with `tracer` and `ref_region`.
#'
#' @param ref_region Reference region
#'
#'   A character string is expected.
#'   If `custom_slope` and `custom_intercept` are specified, this parameter is ignored.
#'   See Details section for accepted values in combination with `tracer` and `ref_region`.
#'
#' @param suvr SUVR value
#'
#'   A numeric value is expected.
#'
#' @param custom_slope Optional slope parameter for custom centiloid calculation formula
#'
#'   A numeric value is expected when provided.
#'   When `custom_slope` is specified (along with `custom_intercept`), this overrides
#'   the standard formula parameters `tracer`, `pipeline`, and `ref_region`.
#'   Default is `NULL`.
#'
#' @param custom_intercept Optional intercept parameter for custom centiloid calculation formula
#'
#'   A numeric value is expected when provided.
#'   When `custom_intercept` is specified (along with `custom_slope`), this overrides
#'   the standard formula parameters `tracer`, `pipeline`, and `ref_region`.
#'   Default is `NULL`.
#'
#' @details
#' The centiloid scale is a standardized quantitative measure for amyloid PET imaging
#' that allows comparison between different tracers and analysis methods. This function
#' converts SUVR values to the centiloid scale based on published conversion
#' equations for specific tracer, pipeline, and reference region combinations.
#'
#' Centiloid is calculated as:
#'
#' Centiloid = \deqn{slope \times SUVR + intercept}
#'
#' where slope and intercept are formula parameters. If `custom_slope` and `custom_intercept`
#' are not specified, this function uses pre-defined slope and intercept based on
#' the user's selections of tracer, pipeline, and reference region.
#
#' The following combinations of tracer, pipeline, and reference region are supported:
#'
#'  - `tracer = "18F-Florbetapir"`, `pipeline = "AVID FBP SUVR PIPELINE"`,
#'    `ref_region = "Whole Cerebellum"`
#'  - `tracer = "18F-Florbetapir"`, `pipeline = "BERKELEY FBP SUVR PIPELINE"`,
#'    `ref_region = "Whole Cerebellum"`
#'  - `tracer = "18F-Florbetaben"`, `pipeline = "BERKELEY FBB SUVR PIPELINE"`,
#'    `ref_region = "Whole Cerebellum"`
#'
#' The equation used for the conversion is based on the following references:
#'  1. https://doi.org/10.1016/j.jalz.2018.06.1353 for 18F-Florbetapir, AVID FBP SUVR PIPELINE,
#'     Whole Cerebellum.
#'  2. ADNI UCBerkeley Amyloid PET methods for 18F-Florbetapir, BERKELEY FBP SUVR PIPELINE,
#'     Whole Cerebellum.
#'  3. ADNI UCBerkeley Amyloid PET methods for 18F-Florbetaben, BERKELEY FBB SUVR PIPELINE,
#'     Whole Cerebellum.
#'
#' Alternatively, the user can override the pre-selection by specifying `custom_slope`
#' and `custom_intercept` instead. If `custom_slope` and `custom_intercept` are specified,
#' `tracer`, `pipeline` and `ref_region` are ignored.
#'
#' If a matching combination of tracer, pipeline and reference region is not specified and
#' `custom_slope` and `custom_intercept` are not specified, a warning is issued and
#' `NA_real_` is returned.
#'
#' @return A numeric centiloid value
#'
#' @keywords com_bds_findings
#' @family com_bds_findings
#'
#' @export
#'
#' @examples
#' # Using standard parameters
#' compute_centiloid(
#'   tracer = "18F-Florbetapir",
#'   pipeline = "AVID FBP SUVR PIPELINE",
#'   ref_region = "Whole Cerebellum",
#'   suvr = 1.25
#' )
#'
#' # Using custom parameters
#' compute_centiloid(
#'   tracer = "MyTracer",
#'   pipeline = "MyPipeline",
#'   ref_region = "MyRegion",
#'   suvr = 1.25,
#'   custom_slope = 193,
#'   custom_intercept = -187
#' )
compute_centiloid <- function(
    tracer,
    pipeline,
    ref_region,
    suvr,
    custom_slope = NULL,
    custom_intercept = NULL) {
  valid_combinations <- tibble::tribble(
    ~tracer,              ~pipeline,                     ~ref_region,          ~slope,   ~intercept,
    "18F-Florbetapir",    "AVID FBP SUVR PIPELINE",      "Whole Cerebellum",   183,      -177,
    "18F-Florbetapir",    "BERKELEY FBP SUVR PIPELINE",  "Whole Cerebellum",   188.22,   -189.16,
    "18F-Florbetaben",    "BERKELEY FBB SUVR PIPELINE",  "Whole Cerebellum",   157.15,   -151.87
  )

  # Check custom_slope and custom_intercept
  has_custom_slope <- !is.null(custom_slope)
  has_custom_intercept <- !is.null(custom_intercept)

  if (has_custom_slope != has_custom_intercept) {
    cli::cli_abort("Both {.code custom_slope} and {.code custom_intercept}
                   be specified together")
  }

  use_custom_params <- has_custom_slope && has_custom_intercept

  if (use_custom_params) {
    assert_numeric_vector(custom_slope, length = 1)
    assert_numeric_vector(custom_intercept, length = 1)
    assert_numeric_vector(suvr, length = 1)

    # Use custom parameters
    slope <- custom_slope
    intercept <- custom_intercept
  } else {
    assert_character_scalar(tracer, values = c("18F-Florbetapir", "18F-Florbetaben"))
    assert_character_scalar(pipeline, values = c(
      "AVID FBP SUVR PIPELINE",
      "BERKELEY FBP SUVR PIPELINE",
      "BERKELEY FBB SUVR PIPELINE"
    ))
    assert_character_scalar(ref_region, values = c(
      "Whole Cerebellum",
      "Composite Reference Region"
    ))

    # Check if the combination exists in valid_combinations
    check <- valid_combinations %>%
      dplyr::filter(
        tracer == !!tracer,
        pipeline == !!pipeline,
        ref_region == !!ref_region
      )

    if (nrow(check) == 0) {
      cli::cli_warn(
        c(
          "No standard conversion formula available for:",
          "i" = "tracer = {.val {tracer}}",
          "i" = "pipeline = {.val {pipeline}}",
          "i" = "ref_region = {.val {ref_region}}"
        )
      )
      return(NA_real_)
    } else {
      slope <- check$slope
      intercept <- check$intercept
    }
  }

  # Calculate centiloid value
  centiloid <- slope * suvr + intercept
  return(centiloid)
}
