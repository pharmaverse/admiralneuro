# ---- Valid combination tests ----
test_that("Test 1: 18F-Florbetapir with AVID pipeline and whole cerebellum ref region", {
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

test_that("Test 2: 18F-Florbetaben with AVID pipeline and whole cerebellum ref region", {
  expect_equal(
    compute_centiloid(
      tracer = "18F-Florbetaben",
      pipeline = "AVID FBB SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.20
    ),
    156.06 * 1.20 - 148.13
  )
})

test_that("Test 3: 18F-Florbetapir with BERKELEY FBP pipeline and whole cerebellum ref region", {
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

test_that("Test 4: 18F-Florbetaben with BERKELEY FBB pipeline and whole cerebellum ref region", {
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

test_that("Test 5: Unrecognized combination triggers error", {
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
test_that("Test 6: Invalid tracer value triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "InvalidTracer",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    )
  )
})

test_that("Test 7: Invalid pipeline value triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "UnknownPipeline",
      ref_region = "Whole Cerebellum",
      suvr = 1.25
    )
  )
})

test_that("Test 8: Invalid reference region triggers error", {
  expect_snapshot_error(
    compute_centiloid(
      tracer = "18F-Florbetapir",
      pipeline = "AVID FBP SUVR PIPELINE",
      ref_region = "InvalidRegion",
      suvr = 1.25
    )
  )
})

test_that("Test 9: Non-numeric SUVR value triggers error", {
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
test_that("Test 10: Custom parameters override standard formula", {
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

test_that("Test 11: Custom parameters with invalid combination still works", {
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

test_that("Test 12: Only one custom parameter provided triggers error", {
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

test_that("Test 13: Custom parameters with missing values triggers error", {
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

test_that("Test 14: Custom parameters non numeric inputs trigger error", {
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

test_that("Test 15 cli_abort() throws the expected error message", {
  # The cli_abort() message uses string interpolation
  # so these must exisit in the test environment
  # to include variable values in the error message.

  tracer <- "TestTracer"
  pipeline <- "TestPipeline"
  ref_region <- "TestRegion"

  expect_snapshot_error(
   cli_abort(c(
      "No standard conversion formula available for:",
      "*" = "tracer = {.val {tracer}}",
      "*" = "pipeline = {.val {pipeline}}",
      "*" = "ref_region = {.val {ref_region}}"
  ))
  )
})
