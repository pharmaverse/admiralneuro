# ---- Valid combination tests ----
## Test 1: 18F-Florbetapir with AVID pipeline and ref_region
test_that("Test 1: 18F-Florbetapir with AVID pipeline", {
  expect_equal(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    ),
    183.07 * 1.25 - 177.26
  )
})

## Test 2: 18F-Florbetapir with BERKELEY pipeline and Whole Cerebellum
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

## Test 3: 18F-Florbetaben with BERKELEY pipeline and Whole Cerebellum
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

test_that("Test 4: Unrecognized combination returns NA with error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Composite Reference Region",
      suvr = 1.25
    )
  )
})

# ---- Input validation ----
## Test 5: Invalid tracer value triggers error
test_that("Test 5: Invalid tracer value triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "InvalidTracer",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    )
  )
})

## Test 6: Invalid pipeline value triggers error
test_that("Test 6: Invalid pipeline value triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "UnknownPipeline",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    )
  )
})

## Test 7: Invalid reference region value triggers error
test_that("Test 7: Invalid reference region triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "InvalidRegion",
      suvr = 1.25
    )
  )
})

test_that("Test 8: Non-numeric SUVR value triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = "1.25"
    )
  )
})

# ---- Custom parameter tests ----
## Test 9: Custom parameters override standard formula
test_that("Test 9: Custom parameters override standard formula", {
  expect_equal(
    compute_centiloid(
      tracer = "AnyTracer",
      pipeline = "AnyPipeline",
      ref_region = "AnyRegion",
      suvr = 1.25,
      custom_slope = 193,
      custom_intercept = -187
    ),
    193 * 1.25 - 187
  )
})

## Test 10: Custom parameters with invalid combination still works
test_that("Test 10: Custom parameters with invalid combination still works", {
  expect_equal(
    compute_centiloid(
      tracer = "InvalidTracer",
      pipeline = "InvalidPipeline",
      ref_region = "InvalidRegion",
      suvr = 1.25,
      custom_slope = 150,
      custom_intercept = -100
    ),
    150 * 1.25 - 100
  )
})

## Test 11: Only one custom parameter provided triggers error
test_that("Test 11: Only one custom parameter provided triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "Tracer",
      pipeline = "Pipeline",
      ref_region = "Region",
      suvr = 1.25,
      custom_slope = 193
    )
  )

  expect_snapshot_error(
    compute_centiloid(
      tracer = "Tracer",
      pipeline = "Pipeline",
      ref_region = "Region",
      suvr = 1.25,
      custom_intercept = -187
    )
  )
})

## Test 12: Custom parameters with missing values triggers error
test_that("Test 12: Custom parameters with missing values triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "Tracer",
      pipeline = "Pipeline",
      ref_region = "Region",
      suvr = 1.25,
      custom_slope = NULL,
      custom_intercept = -187
    )
  )

  expect_snapshot_error(
    compute_centiloid(
      tracer = "Tracer",
      pipeline = "Pipeline",
      ref_region = "Region",
      suvr = 1.25,
      custom_slope = 193,
      custom_intercept = NULL
    )
  )
})

## Test 13: Custom parameters non-numeric inputs triggers error
test_that("Test 13: Custom parameters non numeric inputs", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "Tracer",
      pipeline = "Pipeline",
      ref_region = "Region",
      suvr = 1.25,
      custom_slope = "193",
      custom_intercept = -187
    )
  )

  expect_snapshot_error(
    compute_centiloid(
      tracer = "Tracer",
      pipeline = "Pipeline",
      ref_region = "Region",
      suvr = 1.25,
      custom_slope = 193,
      custom_intercept = "-187"
    )
  )
})
