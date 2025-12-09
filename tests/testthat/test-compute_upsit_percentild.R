# Test: compute_upsit_percentile

# Test 1: Exact value verification for known cases ----
test_that("compute_upsit_percentile returns exact correct percentiles", {
  # Male, age 52 (50-54 range), score 25
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 52, upsit_score = 25),
    5 # Expected percentile value
  )

  # Female, age 67 (65-69 range), score 30
  expect_equal(
    compute_upsit_percentile(sex = "F", age = 67, upsit_score = 30),
    22.5 # Expected percentile value
  )

  # Male, age 58 (55-59 range), score 35
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 58, upsit_score = 35),
    62 # Expected percentile value
  )

  # Female, age 72 (70-74 range), score 22
  expect_equal(
    compute_upsit_percentile(sex = "F", age = 72, upsit_score = 22),
    9 # Expected percentile value
  )
})

# Test 2: Age range boundaries ----
test_that("compute_upsit_percentile handles age range boundaries correctly", {
  # Lower boundary: age 50 (start of 50-54 range)
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 50, upsit_score = 28),
    7 # Expected percentile value
  )

  # Upper boundary: age 54 (end of 50-54 range)
  # Should return same value as age 50 if same score
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 54, upsit_score = 28),
    7 # Expected percentile value (same as age 50)
  )

  # Boundary between ranges: age 55 (start of 55-59 range)
  expect_equal(
    compute_upsit_percentile(sex = "F", age = 55, upsit_score = 28),
    4 # Expected percentile value
  )

  # Upper boundary: age 79 (end of 75-79 range)
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 79, upsit_score = 25),
    24 # Expected percentile value
  )
})

# Test 3: 80+ age group (no upper limit) ----
test_that("compute_upsit_percentile handles 80+ age group correctly", {
  # Age 80 (lower boundary of 80+ range)
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 80, upsit_score = 20),
    20 # Expected percentile value
  )
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 85, upsit_score = 20),
    20 # Expected percentile value (same as age 80)
  )
  expect_equal(
    compute_upsit_percentile(sex = "F", age = 95, upsit_score = 18),
    8 # Expected percentile value
  )
  expect_equal(
    compute_upsit_percentile(sex = "F", age = 100, upsit_score = 18),
    8 # Expected percentile value (same as age 95)
  )
})

# Test 4: Both sexes with same age and score ----
test_that("compute_upsit_percentile differentiates between male and female", {
  # Male, age 60, score 28
  result_male <- compute_upsit_percentile(sex = "M", age = 60, upsit_score = 28)
  expect_equal(result_male, 19)

  # Female, age 60, score 28
  result_female <- compute_upsit_percentile(sex = "F", age = 60, upsit_score = 28)
  expect_equal(result_female, 10)
})

# Test 5: Extreme UPSIT scores ----
test_that("compute_upsit_percentile handles extreme UPSIT scores", {
  # Minimum possible score: 0
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 55, upsit_score = 0),
    1 # Expected percentile value (likely very low, e.g., 1)
  )

  # Maximum possible score: 40
  expect_equal(
    compute_upsit_percentile(sex = "F", age = 65, upsit_score = 40),
    100 # Expected percentile value (likely very high, e.g., 99)
  )

  # Low score in older age group
  expect_equal(
    compute_upsit_percentile(sex = "M", age = 82, upsit_score = 10),
    2.5 # Expected percentile value
  )
})

# Test 6: Invalid inputs return NA ----
test_that("compute_upsit_percentile returns NA for out-of-range age", {
  # Age below minimum (< 50)
  expect_true(is.na(compute_upsit_percentile(sex = "M", age = 45, upsit_score = 25)))
  expect_true(is.na(compute_upsit_percentile(sex = "F", age = 30, upsit_score = 30)))
  expect_true(is.na(compute_upsit_percentile(sex = "M", age = 49, upsit_score = 28)))
})

test_that("compute_upsit_percentile returns NA for invalid sex", {
  # Invalid sex codes
  expect_true(is.na(compute_upsit_percentile(sex = "X", age = 55, upsit_score = 25)))
  expect_true(is.na(compute_upsit_percentile(sex = "Male", age = 55, upsit_score = 25)))
  expect_true(is.na(compute_upsit_percentile(sex = "1", age = 55, upsit_score = 25)))
  expect_true(is.na(compute_upsit_percentile(sex = "", age = 55, upsit_score = 25)))
})

test_that("compute_upsit_percentile returns NA for invalid UPSIT scores", {
  # Score not in lookup table (if your table doesn't have all 0-40)
  # Adjust based on what scores are actually in your lookup table
  expect_true(is.na(compute_upsit_percentile(sex = "M", age = 55, upsit_score = 999)))
  expect_true(is.na(compute_upsit_percentile(sex = "M", age = 55, upsit_score = -5)))

  # If your lookup table doesn't have score 41
  expect_true(is.na(compute_upsit_percentile(sex = "M", age = 55, upsit_score = 41)))
})

# Test 7: NA input handling ----
test_that("compute_upsit_percentile handles NA inputs appropriately", {
  # NA sex
  result <- compute_upsit_percentile(sex = NA_character_, age = 55, upsit_score = 25)
  expect_true(is.na(result))
  expect_type(result, "double") # Should still return numeric NA

  # NA age
  result <- compute_upsit_percentile(sex = "M", age = NA_real_, upsit_score = 25)
  expect_true(is.na(result))
  expect_type(result, "double")

  # NA upsit_score
  result <- compute_upsit_percentile(sex = "M", age = 55, upsit_score = NA_real_)
  expect_true(is.na(result))
  expect_type(result, "double")

  # All NA
  result <- compute_upsit_percentile(sex = NA_character_, age = NA_real_, upsit_score = NA_real_)
  expect_true(is.na(result))
  expect_type(result, "double")
})

# Test 8: Vectorization with multiple inputs ----
test_that("compute_upsit_percentile works correctly with vectorized inputs", {
  # Create test data with known values
  test_df <- data.frame(
    SEX = c("M", "F", "M", "F", "M"),
    AGE = c(52, 67, 58, 82, 75),
    UPSIT_SCORE = c(25, 30, 35, 20, 28)
  )

  # Apply function
  test_df$percentile <- compute_upsit_percentile(
    sex = test_df$SEX,
    age = test_df$AGE,
    upsit_score = test_df$UPSIT_SCORE
  )

  # Check all results are numeric
  expect_type(test_df$percentile, "double")
  expect_equal(length(test_df$percentile), 5)

  # Check specific values
  expect_equal(test_df$percentile[1], 5) # M, 52, 25
  expect_equal(test_df$percentile[2], 22.5) # F, 67, 30
  expect_equal(test_df$percentile[3], 62) # M, 58, 35
  expect_equal(test_df$percentile[4], 11) # F, 82, 20
  expect_equal(test_df$percentile[5], 36) # M, 75, 28
})

# Test 9: Vectorization with some NA inputs ----
test_that("compute_upsit_percentile handles mixed valid and NA inputs in vectors", {
  test_df <- data.frame(
    SEX = c("M", NA_character_, "F", "M"),
    AGE = c(52, 67, NA_real_, 75),
    UPSIT_SCORE = c(25, 30, 28, NA_real_)
  )

  result <- compute_upsit_percentile(
    sex = test_df$SEX,
    age = test_df$AGE,
    upsit_score = test_df$UPSIT_SCORE
  )

  # First should have valid result
  expect_equal(result[1], 5) # M, 52, 25

  # Second, third, fourth should be NA (due to NA inputs)
  expect_true(is.na(result[2])) # NA sex
  expect_true(is.na(result[3])) # NA age
  expect_true(is.na(result[4])) # NA score

  # All should be numeric type
  expect_type(result, "double")
})

# Test 10: Type consistency ----
test_that("compute_upsit_percentile always returns numeric double type", {
  # Valid input returns double
  result <- compute_upsit_percentile(sex = "M", age = 55, upsit_score = 25)
  expect_type(result, "double")

  # Invalid input returns numeric NA (not logical NA)
  result <- compute_upsit_percentile(sex = "M", age = 45, upsit_score = 25)
  expect_type(result, "double")
  expect_true(is.na(result))

  # Vector input returns double vector
  result <- compute_upsit_percentile(
    sex = c("M", "F"),
    age = c(55, 65),
    upsit_score = c(25, 30)
  )
  expect_type(result, "double")
  expect_equal(length(result), 2)
})

# Test 11: All age ranges are covered ----
test_that("compute_upsit_percentile has data for all documented age ranges", {
  # Test one value from each age range to ensure lookup table is complete
  age_test_cases <- list(
    list(age = 52, range = "50-54"),
    list(age = 57, range = "55-59"),
    list(age = 62, range = "60-64"),
    list(age = 67, range = "65-69"),
    list(age = 72, range = "70-74"),
    list(age = 77, range = "75-79"),
    list(age = 85, range = "80+")
  )

  for (test_case in age_test_cases) {
    result <- compute_upsit_percentile(sex = "M", age = test_case$age, upsit_score = 25)
    expect_false(
      is.na(result),
      info = sprintf("Age %d (range %s) should have data", test_case$age, test_case$range)
    )
    expect_type(result, "double")
  }
})

# Test 12: Consistency - same inputs return same outputs ----
test_that("compute_upsit_percentile returns consistent results for repeated calls", {
  # Call function multiple times with same inputs
  result1 <- compute_upsit_percentile(sex = "M", age = 60, upsit_score = 28)
  result2 <- compute_upsit_percentile(sex = "M", age = 60, upsit_score = 28)
  result3 <- compute_upsit_percentile(sex = "M", age = 60, upsit_score = 28)

  # All results should be identical
  expect_equal(result1, result2)
  expect_equal(result2, result3)

  expect_equal(result1, 19)
})
