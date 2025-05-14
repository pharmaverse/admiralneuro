#' Compute Centiloid Value
#'
#' Computes centiloid values based on amyloid PET tracer, SUVR value and pipeline,
#' and reference region.
#' Also allows for custom formula parameters.
#'
#' @param tracer Amyloid PET tracer from ag_neuro dataset
#'
#'   A character string is expected.
#'   When `custom_params` is not provided, accepted values are:
#'   '18F-Florbetapir', '18F-Florbetaben'
#'   If `custom_params` is provided, any string is accepted.
#'
#' @param pipeline SUVR pipeline from nv_neuro dataset
#'
#'   A character string is expected.
#'   When `custom_params` is not provided, accepted values are:
#'   'AVID FBP SUVR PIPELINE', 'BERKELEY FBP SUVR PIPELINE', 'BERKELEY FBB SUVR PIPELINE'
#'   If `custom_params` is provided, any string is accepted.
#'
#' @param ref_region Reference region from suppnv_neuro dataset
#'
#'   A character string is expected.
#'   When `custom_params` is not provided, accepted values are:
#'   'Whole Cerebellum', 'Composite Reference Region'
#'   If `custom_params` is provided, any string is accepted.
#'
#' @param suvr SUVR value
#'
#'   A numeric value is expected.
#'
#' @param custom_params Optional list containing custom formula parameters 'm' and 'c'
#'
#'   A list with two named numeric values is expected when provided:
#'   list(m = numeric_value, c = numeric_value)
#'   When provided, this overrides the standard formula parameters
#'
#' @details
#' The centiloid scale is a standardized quantitative measure for amyloid PET imaging
#' that allows comparison between different tracers and analysis methods. This function
#' converts SUVR values to the centiloid scale based on published conversion
#' equations for specific tracer, pipeline, and reference region combinations.
#'
#' Centiloid is calculated as:
#'
#' Centiloid = m * SUVR + c
#'
#' where m and c are formula parameters. If `custom_params` is provided, this formula
#' is used regardless of the tracer, pipeline, or reference region values.
#'
#' If a matching standard combination is not found and no `custom_params` are provided,
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
#'   custom_params = list(m = 193, c = -187)
#' )
compute_centiloid <- function(tracer, pipeline, ref_region, suvr, custom_params = NULL) {
  params_map <- list(
    # reference: https://doi.org/10.1016/j.jalz.2018.06.1353
    "18F-Florbetapir | AVID FBP SUVR PIPELINE | Whole Cerebellum"      =
      list(m = 183, c = -177),
    # reference: ADNI UCBerkeley Amyloid PET methods
    "18F-Florbetapir | BERKELEY FBP SUVR PIPELINE | Whole Cerebellum"  =
      list(m = 188.22, c = -189.16),
    # reference: ADNI UCBerkeley Amyloid PET methods
    "18F-Florbetaben | BERKELEY FBB SUVR PIPELINE | Whole Cerebellum"  =
      list(m = 157.15, c = -151.87)
  )

  build_key <- function(tracer, pipeline, ref_region) {
    paste(tracer, pipeline, ref_region, sep = " | ")
  }

  # Input validation
  if (is.null(custom_params)) {
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

  # Custom params validation if provided
  if (!is.null(custom_params)) {
    if (!is.list(custom_params)) {
      stop("custom_params must be a list with 'm' and 'c' components")
    }
    if (!all(c("m", "c") %in% names(custom_params))) {
      stop("custom_params must contain both 'm' and 'c' components")
    }
    if (!is.numeric(custom_params$m) || !is.numeric(custom_params$c)) {
      stop("custom_params 'm' and 'c' must be numeric values")
    }

    # Use custom parameters
    params <- list(m = custom_params$m, c = custom_params$c)
  } else {
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

  # Calculate centiloid value
  centiloid <- params$m * suvr + params$c
  return(centiloid)
}
