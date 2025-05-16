#' Compute Centiloid Value
#'
#' Computes centiloid values based on amyloid PET tracer, SUVR value and pipeline,
#' and reference region.
#' Also allows for custom formula parameters.
#'
#' @param tracer Amyloid PET tracer from ag_neuro dataset
#'
#'   A character string is expected.
#'   When custom parameters are not provided, accepted values are:
#'   '18F-Florbetapir', '18F-Florbetaben'
#'   If custom slope and intercept are provided, any string is accepted.
#'
#' @param pipeline SUVR pipeline from nv_neuro dataset
#'
#'   A character string is expected.
#'   When custom parameters are not provided, accepted values are:
#'   'AVID FBP SUVR PIPELINE', 'BERKELEY FBP SUVR PIPELINE', 'BERKELEY FBB SUVR PIPELINE'
#'   If custom slope and intercept are provided, any string is accepted.
#'
#' @param ref_region Reference region from suppnv_neuro dataset
#'
#'   A character string is expected.
#'   When custom parameters are not provided, accepted values are:
#'   'Whole Cerebellum', 'Composite Reference Region'
#'   If custom slope and intercept are provided, any string is accepted.
#'
#' @param suvr SUVR value
#'
#'   A numeric value is expected.
#'
#' @param custom_slope Optional slope parameter for custom centiloid calculation formula
#'
#'   A numeric value is expected when provided.
#'   When provided with custom_slope, this overrides the standard formula parameters.
#'   Default is NULL.
#'
#' @param custom_intercept Optional intercept parameter for custom centiloid calculation formula
#'
#'   A numeric value is expected when provided.
#'   When provided with custom_intercept, this overrides the standard formula parameters.
#'   Default is NULL.
#'
#' @details
#' The centiloid scale is a standardized quantitative measure for amyloid PET imaging
#' that allows comparison between different tracers and analysis methods. This function
#' converts SUVR values to the centiloid scale based on published conversion
#' equations for specific tracer, pipeline, and reference region combinations.
#'
#' Centiloid is calculated as:
#'
#' Centiloid = slope * SUVR + intercept
#'
#' where slope and intercept are formula parameters. If custom slope and intercept are provided,
#' this formula is used regardless of the tracer, pipeline, or reference region values.
#'
#' If a matching standard combination is not found and no custom parameters are provided,
#' a warning is issued and `NA_real_` is returned.
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
    custom_intercept = NULL
) {
  params_map <- list(
    # reference: https://doi.org/10.1016/j.jalz.2018.06.1353
    "18F-Florbetapir | AVID FBP SUVR PIPELINE | Whole Cerebellum" =
      list(slope = 183, intercept = -177),
    # reference: ADNI UCBerkeley Amyloid PET methods
    "18F-Florbetapir | BERKELEY FBP SUVR PIPELINE | Whole Cerebellum" =
      list(slope = 188.22, intercept = -189.16),
    # reference: ADNI UCBerkeley Amyloid PET methods
    "18F-Florbetaben | BERKELEY FBB SUVR PIPELINE | Whole Cerebellum" =
      list(slope = 157.15, intercept = -151.87)
  )

  build_key <- function(tracer, pipeline, ref_region) {
    paste(tracer, pipeline, ref_region, sep = " | ")
  }

  # 1. Check that all required parameters are provided
  if (missing(tracer) || is.null(tracer)) {
    stop("tracer parameter is required")
  }
  if (missing(pipeline) || is.null(pipeline)) {
    stop("pipeline parameter is required")
  }
  if (missing(ref_region) || is.null(ref_region)) {
    stop("ref_region parameter is required")
  }
  if (missing(suvr) || is.null(suvr)) {
    stop("suvr parameter is required")
  }

  # 2. Check custom parameters
  has_custom_slope <- !is.null(custom_slope) & is.numeric(custom_slope)
  has_custom_intercept <- !is.null(custom_intercept) & is.numeric(custom_intercept)

  # Both custom parameters must be provided together
  if (has_custom_slope != has_custom_intercept) {
    stop("Both numeric custom_slope and custom_intercept must be provided together")
  }

  us_custom_params <- has_custom_slope && has_custom_intercept

  # 3. Input validation
  if (!use_custom_params) {
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
  } else {
    assert_character_scalar(tracer)
    assert_character_scalar(pipeline)
    assert_character_scalar(ref_region)
  }
  assert_numeric_vector(suvr, length = 1)

  # 4. Prepare parameters for calculation
  if (us_custom_params) {
    params <- list(slope = custom_slope, intercept = custom_intercept)
  } else {
    # Use standard parameters based on tracer, pipeline, and reference region
    key <- build_key(tracer, pipeline, ref_region)
    params <- params_map[[key]]

    if (is.null(params)) {
      warning(
        paste(
          "No standard conversion formula available for:",
          "tracer =", tracer,
          "| pipeline =", pipeline,
          "| ref_region =", ref_region
        )
      )
      return(NA_real_)
    }
  }

  # 5. Calculate centiloid value
  centiloid <- params$slope * suvr + params$intercept
  return(centiloid)
}
