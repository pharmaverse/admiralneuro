#' Compute UPSIT Percentile Based on Age, Sex, and Score
#'
#' This function computes the UPSIT (University of Pennsylvania Smell
#' Identification Test) percentile for a given subject based on their age,
#' sex, and UPSIT raw score. The percentile is determined by looking up the
#' corresponding value in a normative reference table.
#'
#' @param sex Character. Sex of the subject. Must be either "M" (male) or
#'   "F" (female).
#' @param age Numeric. Age of the subject in years. The function handles
#'   age ranges including "80+" for subjects aged 80 and above.
#' @param upsit_score Numeric. The UPSIT score, which ranges from 0 to 40.
#'   Higher scores indicate better olfactory function.
#'
#' @return Numeric. The UPSIT percentile value corresponding to the input
#'   parameters. Returns `NA_real_` if no matching entry is found in the
#'   lookup table.
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
#' @examples
#' # Single value computation
#' compute_upsit_percentile(sex = "M", age = 52, upsit_score = 25)
#'
#' # Female subject aged 81 (in the 80+ category)
#' compute_upsit_percentile(sex = "F", age = 81, upsit_score = 30)
#'
#' # Returns NA if no match found (e.g., age out of range)
#' compute_upsit_percentile(sex = "M", age = 45, upsit_score = 25)
#'
#' \dontrun{
#' # Usage in admiral workflow with derive_extreme_records
#' library(admiral)
#' library(dplyr)
#'
#' adapet <- adapet %>%
#'   derive_extreme_records(
#'     dataset = .,
#'     dataset_add = .,
#'     filter_add = (PARAMCD == "UPSIT"),
#'     set_values_to = exprs(
#'       AVAL = compute_upsit_percentile(
#'         sex = SEX,
#'         age = AGE,
#'         upsit_score = AVAL
#'       ),
#'       PARAMCD = "UPSITPCT",
#'       PARAM = "UPSIT Percentile",
#'       AVALU = "Percentile"
#'     ),
#'     keep_source_vars = exprs(USUBJID, VISIT, VISITNUM)
#'   )
#'
#' # Usage with dplyr
#' df <- df %>%
#'   mutate(
#'     upsit_percentile = compute_upsit_percentile(
#'       sex = SEX,
#'       age = AGE,
#'       upsit_score = UPSIT_SCORE
#'     )
#'   )
#' }
#'
#' @export
compute_upsit_percentile <- function(sex, age, upsit_score) {

  # Vectorized implementation - handle multiple inputs at once
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

  return(result)
}
