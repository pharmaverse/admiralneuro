# Compute UPSIT Percentile Based on Age, Sex, and Score

This function computes the UPSIT (University of Pennsylvania Smell
Identification Test) percentile for a given subject based on their age,
sex, and UPSIT raw score. The percentile is determined by looking up the
corresponding value in a normative reference table.

## Usage

``` r
compute_upsit_percentile(sex, age, upsit_score)
```

## Arguments

- sex:

  Sex of the subject. Must be either "M" or "F" A character string of
  either "M" or "F" is expected

  Permitted values

  :   a character scalar, i.e., a character vector of length one

  Default value

  :   none

- age:

  Age of the subject in years.

  Permitted values

  :   a numeric scalar, i.e., a numeric vector of length one

  Default value

  :   none

- upsit_score:

  The UPSIT ranging from 0 to 40. Higher scores indicate better
  olfactory function.

  Permitted values

  :   a numeric scalar, i.e., a numeric vector of length one

  Default value

  :   none

## Value

A numeric percentile value. The UPSIT percentile value corresponding to
the input parameters. Returns `NA_real_` if no matching entry is found
in the lookup table.

## Details

The function uses an internal lookup table (`upsit_lookup`) that
contains normative percentile data stratified by sex, age range, and
UPSIT score. The lookup table is based on published normative data.

Age ranges in the lookup table include:

- 50-54 years

- 55-59 years

- 60-64 years

- 65-69 years

- 70-74 years

- 75-79 years

- 80+ years (no upper limit)

The function is designed to work efficiently in vectorized operations
within data processing pipelines (e.g., with
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html))

## References

Brumm MC, et. al., Updated Percentiles for the University of
Pennsylvania Smell Identification Test in Adults 50 Years of Age and
Older.
[doi:10.1212/WNL.0000000000201463](https://doi.org/10.1212/WNL.0000000000201463)

## See also

Other com_bds_findings:
[`compute_centiloid()`](https://pharmaverse.github.io/admiralneuro/reference/compute_centiloid.md)

## Examples

### Look up for male percentile

A 52 years old male with upsit raw score of 25

    compute_upsit_percentile(sex = "M", age = 52, upsit_score = 25)
    #> [1] 5

### Look up for female percentile

A 81 years old female with upsit raw score of 30

    compute_upsit_percentile(sex = "F", age = 81, upsit_score = 30)
    #> [1] 39

### Returns NA

Minimal age is 50 and score of 0 and 40, return NA if no match found

    compute_upsit_percentile(sex = "M", age = 45, upsit_score = 25)
    #> [1] NA
