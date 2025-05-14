# test-compute_centiloid.R

# ---- Standard parameter tests ----

test_that("Test 1: 18F-Florbetapir with AVID pipeline", {
  expect_equal(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    ),
    183 * 1.25 - 177
  )
})

test_that("Test 2: 18F-Florbetapir with BERKELEY FBP pipeline", {
  expect_equal(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "BERKELEY FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.10
    ),
    188.22 * 1.10 - 189.16
  )
})

test_that("Test 3: 18F-Florbetaben with BERKELEY FBB pipeline", {
  expect_equal(
    compute_centiloid(
      tracer = "18F-Florbetaben",
      pipeline = "BERKELEY FBB SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.30
    ),
    157.15 * 1.30 - 151.87
  )
})

test_that("Test 4: Unrecognized combination returns NA with warning", {
  expect_warning(
    result <- compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Composite Reference Region",
      suvr = 1.25
    )
  )
  expect_true(is.na(result))
})

# ---- Input validation ----

test_that("Test 5: Invalid tracer value triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "InvalidTracer",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    )
  )
})

test_that("Test 6: Invalid pipeline value triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "UnknownPipeline",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    )
  )
})

test_that("Test 7: Invalid reference region triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "InvalidRegion",
      suvr = 1.25
    )
  )
})

test_that("Test 8: Non-numeric SUVr value triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = "1.25"
    )
  )
})

# ---- Custom parameter tests ----

test_that("Test 9: Custom parameters override standard formula", {
  expect_equal(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25,
      custom_params = list(m = 193, c = -187)
    ),
    193 * 1.25 - 187
  )
})

test_that("Test 10: Custom parameters with invalid combination still works", {
  expect_equal(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "Unknown",
      ref_region = "Composite Reference Region",
      suvr = 1.25,
      custom_params = list(m = 150, c = -100)
    ),
    150 * 1.25 - 100
  )
})

test_that("Test 11: custom_params not a list triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25,
      custom_params = c(m = 193, c = -187)
    )
  )
})

test_that("Test 12: custom_params missing 'c' component triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25,
      custom_params = list(m = 193)
    )
  )
})

test_that("Test 13: custom_params missing 'm' component triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25,
      custom_params = list(c = -187)
    )
  )
})

test_that("Test 14: custom_params with non-numeric 'm' triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25,
      custom_params = list(m = "193", c = -187)
    )
  )
})

test_that("Test 15: custom_params with non-numeric 'c' triggers error", {
  expect_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25,
      custom_params = list(m = 193, c = "-187")
    )
  )
})
