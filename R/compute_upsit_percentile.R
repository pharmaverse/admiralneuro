#' Compute UPSIT Percentile Based on Age, Sex, and Score
#'
#' This function computes the UPSIT (University of Pennsylvania Smell
#' Identification Test) percentile for a given subject based on their age,
#' sex, and UPSIT raw score. The percentile is determined by looking up the
#' corresponding value in a normative reference table.
#'
#' @md
#'
#' @param sex Sex of the subject. Must be either "M" or "F"
#'  A character string of either "M" or "F" is expected
#'
#' @permitted [char_scalar]
#'
#' @param age Age of the subject in years.
#'
#' @permitted [num_scalar]
#'
#' @param upsit_score The UPSIT ranging from 0 to 40.
#'   Higher scores indicate better olfactory function.
#'
#' @permitted [num_scalar]
#'
#' @details
#' The function uses an internal lookup table (`upsit_lookup`) that contains
#' normative percentile data stratified by sex, age range, and UPSIT score.
#' The lookup table is based on published normative data.
#'
#' Age ranges in the lookup table include:
#' \itemize{
#'   \item 50-54 years
#'   \item 55-59 years
#'   \item 60-64 years
#'   \item 65-69 years
#'   \item 70-74 years
#'   \item 75-79 years
#'   \item 80+ years (no upper limit)
#' }
#'
#' The function is designed to work efficiently in vectorized operations
#' within data processing pipelines (e.g., with `dplyr::mutate()`)
#'
#' @references
#' Brumm MC, Pierz KA, Lafontant DE, Caspell-Garcia C, Coffey CS,
#' Siderowf A, Marek K. Updated Percentiles for the University of
#' Pennsylvania Smell Identification Test in Adults 50 Years of Age and Older.
#' \doi{10.1212/WNL.0000000000201463}
#'
#' @return A numeric percentile value.
#'  The UPSIT percentile value corresponding to the input
#'  parameters. Returns `NA_real_` if no matching entry is found in the
#'  lookup table.
#'
#' @keywords com_bds_findings
#' @family com_bds_findings
#'
#' @export
#'
#' @examplesx
#' @caption Look up for male percentile
#' @info A 52 years old male with upsit raw score of 25
#' @code compute_upsit_percentile(sex = "M", age = 52, upsit_score = 25)
#'
#' @caption Look up for female percentile
#' @info A 81 years old female with upsit raw score of 30
#' @code compute_upsit_percentile(sex = "F", age = 81, upsit_score = 30)
#'
#' @caption Returns NA
#' @info Minimal age is 50 and score of 0 and 40, return NA if no match found
#' @code compute_upsit_percentile(sex = "M", age = 45, upsit_score = 25)
#'
compute_upsit_percentile <- function(sex, age, upsit_score) {
  # Vectorized implementation
  n <- length(sex)
  result <- rep(NA_real_, n)

  # Process each element
  for (i in seq_len(n)) {
    # Find matching row in lookup table
    # Handle age_high = NA for "80+" cases
    idx <- which(
      upsit_lookup$sex == sex[i] &
        upsit_lookup$upsit == upsit_score[i] &
        age[i] >= upsit_lookup$age_low &
        (is.na(upsit_lookup$age_high) | age[i] <= upsit_lookup$age_high)
    )

    # Assign percentile if match found
    if (length(idx) > 0) {
      result[i] <- upsit_lookup$upsit_pctl[idx[1]]
    }
  }

  result
}
