# Test 5: Unrecognized combination triggers error

    Argument `ref_region` must be equal to one of "Whole Cerebellum".

# Test 6: Invalid tracer value triggers error

    Argument `tracer` must be equal to one of "18F-Florbetapir" or "18F-Florbetaben".

# Test 7: Invalid pipeline value triggers error

    Argument `pipeline` must be equal to one of "AVID FBP SUVR PIPELINE", "AVID FBB SUVR PIPELINE", "BERKELEY FBP SUVR PIPELINE", or "BERKELEY FBB SUVR PIPELINE".

# Test 8: Invalid reference region triggers error

    Argument `ref_region` must be equal to one of "Whole Cerebellum".

# Test 9: Non-numeric SUVR value triggers error

    non-numeric argument to binary operator

# Test 12: Only one custom parameter provided triggers error

    Both `custom_slope` and `custom_intercept` must be specified together

---

    Both `custom_slope` and `custom_intercept` must be specified together

# Test 13: Custom parameters with missing values triggers error

    Both `custom_slope` and `custom_intercept` must be specified together

---

    Both `custom_slope` and `custom_intercept` must be specified together

# Test 14: Custom parameters non numeric inputs trigger error

    Argument `custom_slope` must be a numeric vector, but it is a string.

---

    Argument `custom_intercept` must be a numeric vector, but it is a string.

# Test 15 cli_abort() throws the expected error message

    No standard conversion formula available for:
    * tracer = "TestTracer"
    * pipeline = "TestPipeline"
    * ref_region = "TestRegion"

