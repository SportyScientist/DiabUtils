test_that("classify_glycemia handles standard ADA classifications correctly", {
  # NGT case
  expect_equal(classify_glycemia(90, 130, 5.5, system = "ADA", details = FALSE), "NGT")
  
  # iIFG case with details
  expect_equal(classify_glycemia(110, 130, 5.5, system = "ADA", details = TRUE), "iIFG")
  
  # iIGT case with details
  expect_equal(classify_glycemia(90, 150, 5.5, system = "ADA", details = TRUE), "iIGT")
  
  # Combined IFG+IGT case with details
  expect_equal(classify_glycemia(110, 150, 5.5, system = "ADA", details = TRUE), "IFG+IGT")
  
  # HBA1C case with details (only when use_hba1c_pre is TRUE)
  expect_equal(classify_glycemia(90, 130, 6.0, system = "ADA", details = TRUE, use_hba1c_pre = TRUE), "HBA1C")
  expect_equal(classify_glycemia(90, 130, 6.0, system = "ADA", details = TRUE, use_hba1c_pre = FALSE), "NGT")
  
  # PRE case without details
  expect_equal(classify_glycemia(90, 150, 5.5, system = "ADA", details = FALSE), "PRE")
  
  # DIA case with high glucose_000
  expect_equal(classify_glycemia(130, 130, 5.5, system = "ADA", details = FALSE), "DIA")
  
  # DIA case with high glucose_120
  expect_equal(classify_glycemia(90, 210, 5.5, system = "ADA", details = FALSE), "DIA")
  
  # DIA case with high hba1c
  expect_equal(classify_glycemia(90, 130, 6.6, system = "ADA", details = FALSE), "DIA")
})

test_that("classify_glycemia handles standard WHO classifications correctly", {
  # NGT case
  expect_equal(classify_glycemia(100, 130, system = "WHO", details = FALSE), "NGT")
  
  # iIFG case with details
  expect_equal(classify_glycemia(115, 130, system = "WHO", details = TRUE), "iIFG")
  
  # iIGT case with details
  expect_equal(classify_glycemia(100, 150, system = "WHO", details = TRUE), "iIGT")
  
  # Combined IFG+IGT case with details
  expect_equal(classify_glycemia(115, 150, system = "WHO", details = TRUE), "IFG+IGT")
  
  # PRE case without details
  expect_equal(classify_glycemia(100, 150, system = "WHO", details = FALSE), "PRE")
  
  # DIA case with high glucose_000
  expect_equal(classify_glycemia(130, 130, system = "WHO", details = FALSE), "DIA")
  
  # DIA case with high glucose_120
  expect_equal(classify_glycemia(100, 210, system = "WHO", details = FALSE), "DIA")
  
  # DIA case with high hba1c (only when use_hba1c_dia is TRUE)
  expect_equal(classify_glycemia(100, 130, 6.6, system = "WHO", details = FALSE, use_hba1c_dia = TRUE), "DIA")
  expect_equal(classify_glycemia(100, 130, 6.6, system = "WHO", details = FALSE, use_hba1c_dia = FALSE), "NGT")
})

test_that("classify_glycemia handles unit conversions correctly", {
  # Test SI units for glucose
  # NGT in conventional units
  conventional_result <- classify_glycemia(90, 130, 5.5, system = "ADA", details = FALSE, unit_glucose = "conventional")
  # Same values but in SI units (mmol/L)
  si_result <- classify_glycemia(90/18.016, 130/18.016, 5.5, system = "ADA", details = FALSE, unit_glucose = "SI")
  expect_equal(conventional_result, si_result)
  
  # Test SI units for hba1c
  # Using conventional units (percentage)
  conventional_result <- classify_glycemia(90, 130, 6.0, system = "ADA", details = FALSE, unit_hba1c = "perc")
  # Same value but in SI units (mmol/mol)
  si_value <- (6.0 - 2.15) / 0.0915
  si_result <- classify_glycemia(90, 130, si_value, system = "ADA", details = FALSE, unit_hba1c = "SI")
  expect_equal(conventional_result, si_result)
})

test_that("classify_glycemia handles error conditions correctly", {
  # Invalid system parameter
  expect_error(classify_glycemia(90, 130, 5.5, system = "XYZ"))
  
  # Missing hba1c for ADA system when use_hba1c_pre is TRUE
  expect_error(classify_glycemia(90, 130, system = "ADA", use_hba1c_pre = TRUE))
  
  # Invalid glucose values
  expect_error(classify_glycemia(-10, 130, 5.5, system = "ADA"))
  expect_error(classify_glycemia(90, -10, 5.5, system = "ADA"))
  
  # Invalid hba1c values
  expect_error(classify_glycemia(90, 130, -1, system = "ADA"))
  
  # Invalid unit parameters
  expect_error(classify_glycemia(90, 130, 5.5, system = "ADA", unit_glucose = "invalid"))
  expect_error(classify_glycemia(90, 130, 5.5, system = "ADA", unit_hba1c = "invalid"))
  
  # Invalid logical parameters
  expect_error(classify_glycemia(90, 130, 5.5, system = "ADA", use_hba1c_pre = "TRUE"))
  expect_error(classify_glycemia(90, 130, 5.5, system = "ADA", use_hba1c_dia = "TRUE"))
})

test_that("classify_glycemia handles vectorized inputs correctly", {
  # Vector of fasting glucose values
  glucose_000 <- c(90, 110, 130)
  glucose_120 <- c(130, 150, 130)
  hba1c <- c(5.5, 5.5, 5.5)
  
  expected_results <- c("NGT", "IFG+IGT", "DIA")
  
  # Test with vectorized inputs
  results <- classify_glycemia(glucose_000, glucose_120, hba1c, system = "ADA", details = TRUE)
  expect_equal(results, expected_results)
  
  # Test with WHO system and NULL hba1c
  results_who <- classify_glycemia(glucose_000, glucose_120, system = "WHO", details = TRUE)
  expect_equal(length(results_who), length(glucose_000))
})

test_that("classify_glycemia handles NA values correctly", {
  # NA values in inputs
  expect_equal(length(classify_glycemia(c(90, NA), c(130, 150), c(5.5, 5.5), system = "ADA")), 2)
  expect_equal(is.na(classify_glycemia(c(90, NA), c(130, 150), c(5.5, 5.5), system = "ADA")[2]), TRUE)
  
  expect_equal(is.na(classify_glycemia(90, NA, 5.5, system = "ADA")), TRUE)
  expect_equal(is.na(classify_glycemia(NA, 130, 5.5, system = "ADA")), TRUE)
  expect_equal(is.na(classify_glycemia(90, 130, NA, system = "ADA", use_hba1c_pre = TRUE)), TRUE)
  expect_equal(is.na(classify_glycemia(90, 130, NA, system = "ADA", use_hba1c_pre = FALSE)), FALSE)
})

test_that("classify_glycemia handles single values correctly", {
  # Test with single values
  expect_equal(classify_glycemia(90, 130, 5.5, system = "ADA", details = FALSE), "NGT")
  expect_equal(classify_glycemia(110, 130, 5.5, system = "ADA", details = TRUE), "iIFG")
  expect_equal(classify_glycemia(110, 150, 5.5, system = "ADA", details = TRUE), "IFG+IGT")
  expect_equal(classify_glycemia(130, 130, 5.5, system = "ADA", details = FALSE), "DIA")
})
  