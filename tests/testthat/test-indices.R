test_that("calculate_HOMAIR works correctly", {
  # Test with standard inputs (SI units)
  expect_equal(calculate_HOMAIR(60, 5, unit_insulin = "SI", unit_glucose = "SI"), (60/6) * 5 / 22.5)
  
  # Test with standard inputs (conventional units)
  expect_equal(calculate_HOMAIR(10, 100, unit_insulin = "conventional", unit_glucose = "conventional"), 10 * (100/18.016) / 22.5)
  
  # Test unit conversions
  # SI insulin, conventional glucose
  expect_equal(calculate_HOMAIR(60, 100, unit_insulin = "SI", unit_glucose = "conventional"), (60/6) * (100/18.016) / 22.5)
  
  # Conventional insulin, SI glucose
  expect_equal(calculate_HOMAIR(10, 5, unit_insulin = "conventional", unit_glucose = "SI"), 10 * 5 / 22.5)
  
  # Test error handling
  expect_error(calculate_HOMAIR(60, 100, unit_insulin = "invalid", unit_glucose = "conventional"))
  expect_error(calculate_HOMAIR(60, 100, unit_insulin = "SI", unit_glucose = "invalid"))
  
  # Test vectorized inputs
  insulin_values <- c(60, 120)
  glucose_values <- c(5, 6)
  expected_results <- c((60/6) * 5 / 22.5, (120/6) * 6 / 22.5)
  expect_equal(calculate_HOMAIR(insulin_values, glucose_values, unit_insulin = "SI", unit_glucose = "SI"), expected_results)
})

test_that("calculate_HOMAB works correctly", {
  # Test with standard inputs (SI units)
  expect_equal(calculate_HOMAB(60, 5, unit_insulin = "SI", unit_glucose = "SI"), (20 * (60/6)) / (5 - 3.5))
  
  # Test with standard inputs (conventional units)
  expect_equal(calculate_HOMAB(10, 100, unit_insulin = "conventional", unit_glucose = "conventional"), (20 * 10) / ((100/18.016) - 3.5))
  
  # Test edge case - glucose value at 3.5
  expect_true(is.na(calculate_HOMAB(60, 3.5, unit_insulin = "SI", unit_glucose = "SI")))
  
  # Test vectorized inputs
  insulin_values <- c(60, 120)
  glucose_values <- c(5, 6)
  expected_results <- c((20 * (60/6)) / (5 - 3.5), (20 * (120/6)) / (6 - 3.5))
  expect_equal(calculate_HOMAB(insulin_values, glucose_values, unit_insulin = "SI", unit_glucose = "SI"), expected_results)
  
  # Test with NA values
  expect_true(is.na(calculate_HOMAB(NA, 5, unit_insulin = "SI", unit_glucose = "SI")))
  expect_true(is.na(calculate_HOMAB(60, NA, unit_insulin = "SI", unit_glucose = "SI")))
})

test_that("calculate_FLI works correctly", {
  # Test with standard inputs (conventional units)
  triglycerides <- 100
  bmi <- 25
  ggt <- 30
  waist <- 80
  
  # Calculate expected result
  numerator <- exp((0.953 * log(triglycerides)) + (0.139 * bmi) + (0.718 * log(ggt)) + (0.053 * waist) - 15.745)
  denominator <- 1 + numerator
  expected_result <- (numerator / denominator) * 100
  
  expect_equal(calculate_FLI(triglycerides, bmi, ggt, waist), expected_result)
  
  # Test with SI units for triglycerides
  si_triglycerides <- triglycerides / 88.57
  expect_equal(
    calculate_FLI(si_triglycerides, bmi, ggt, waist, unit_triglycerides = "SI"),
    expected_result
  )
  
  # Test with SI units for GGT
  si_ggt <- ggt / 60
  expect_equal(
    calculate_FLI(triglycerides, bmi, si_ggt, waist, unit_ggt = "SI"),
    expected_result
  )
  
  # Test with both SI units
  expect_equal(
    calculate_FLI(si_triglycerides, bmi, si_ggt, waist, unit_triglycerides = "SI", unit_ggt = "SI"),
    expected_result
  )
  
  # Test error handling
  expect_error(calculate_FLI(triglycerides, bmi, ggt, waist, unit_triglycerides = "invalid"))
  expect_error(calculate_FLI(triglycerides, bmi, ggt, waist, unit_ggt = "invalid"))
})

test_that("calculate_auc works correctly", {
  # Test with standard inputs
  expect_equal(calculate_auc(10, 20, 60), (10 + 20) * 60 / 2)
  
  # Test with one value being zero
  expect_equal(calculate_auc(0, 20, 60), (0 + 20) * 60 / 2)
  
  # Test with vectorized inputs
  tp1_values <- c(10, 20)
  tp2_values <- c(20, 30)
  time_values <- c(60, 120)
  expected_results <- c((10 + 20) * 60 / 2, (20 + 30) * 120 / 2)
  expect_equal(calculate_auc(tp1_values, tp2_values, time_values), expected_results)
})

test_that("calculate_egfr works correctly", {
  # Test for female
  creatinine_f <- 0.6
  age_f <- 50
  sex_f <- "female"
  
  # Expected result for female with creatinine <= 0.7
  expected_f <- 144 * (creatinine_f / 0.7) ** (-0.329) * (0.993 ** age_f)
  expect_equal(calculate_egfr(creatinine_f, age_f, sex_f), expected_f)
  
  # Test for female with creatinine > 0.7
  creatinine_f2 <- 0.8
  expected_f2 <- 144 * (creatinine_f2 / 0.7) ** (-1.209) * (0.993 ** age_f)
  expect_equal(calculate_egfr(creatinine_f2, age_f, sex_f), expected_f2)
  
  # Test for male
  creatinine_m <- 0.8
  age_m <- 50
  sex_m <- "male"
  
  # Expected result for male with creatinine <= 0.9
  expected_m <- 141 * (creatinine_m / 0.9) ** (-0.411) * (0.993 ** age_m)
  expect_equal(calculate_egfr(creatinine_m, age_m, sex_m), expected_m)
  
  # Test for male with creatinine > 0.9
  creatinine_m2 <- 1.0
  expected_m2 <- 141 * (creatinine_m2 / 0.9) ** (-1.209) * (0.993 ** age_m)
  expect_equal(calculate_egfr(creatinine_m2, age_m, sex_m), expected_m2)
  
  # Test with custom female parameter
  creatinine_f <- 0.6
  age_f <- 50
  sex_f <- "F"
  expect_equal(calculate_egfr(creatinine_f, age_f, sex_f, female = "F"), expected_f)
  
  # Test with vectorized inputs
  creatinine_values <- c(0.6, 0.8, 0.8, 1.0)
  age_values <- c(50, 50, 50, 50)
  sex_values <- c("female", "female", "male", "male")
  expected_results <- c(expected_f, expected_f2, expected_m, expected_m2)
  expect_equal(calculate_egfr(creatinine_values, age_values, sex_values), expected_results)
})

test_that("calculate_fib4 works correctly", {
  # Test with standard inputs (conventional units)
  age <- 55
  ast <- 45
  alt <- 30
  platelets <- 220
  
  # Expected result
  expected_result <- (age * ast) / (platelets * sqrt(alt))
  expect_equal(calculate_fib4(age, ast, alt, platelets), expected_result)
  
  # Test with SI units
  si_ast <- ast / 60
  si_alt <- alt / 60
  expect_equal(calculate_fib4(age, si_ast, si_alt, platelets, unit = "SI"), expected_result)
  
  # Test error handling
  expect_error(calculate_fib4(age, ast, alt, platelets, unit = "invalid"))
  
  # Test vectorized inputs
  age_values <- c(55, 60)
  ast_values <- c(45, 50)
  alt_values <- c(30, 35)
  platelets_values <- c(220, 200)
  expected_results <- c(
    (age_values[1] * ast_values[1]) / (platelets_values[1] * sqrt(alt_values[1])),
    (age_values[2] * ast_values[2]) / (platelets_values[2] * sqrt(alt_values[2]))
  )
  expect_equal(calculate_fib4(age_values, ast_values, alt_values, platelets_values), expected_results)
}) 