test_that("prep_data_km prepares data correctly", {
  # Create sample test data
  set.seed(28051996)
  n_patients <- 5
  n_visits_per_patient <- 4
  
  # Create test data with multiple rows per patient
  ids <- rep(1:n_patients, each = n_visits_per_patient)
  dates <- as.Date("2020-01-01") + rep(c(0, 90, 180, 270), times = n_patients)
  
  # Create events - last patient has an event
  events <- rep(0, length(ids))
  events[length(events)] <- 1  # Last visit of last patient has an event
  
  # Create test dataframe
  test_data <- data.frame(
    patno = ids,
    datum = dates,
    event = events
  )
  
  # Skip test if dplyr is not available
  skip_if_not_installed("dplyr")
  
  # Run the function
  result <- suppressWarnings(prep_data_km(test_data, id = "patno", date = "datum", event = "event"))
  
  # Test that output has one row per patient
  expect_equal(nrow(result), n_patients)
  
  # Test that the event is preserved
  expect_equal(sum(result$event), 1)
  
  # Test that time_from_first is calculated correctly
  expect_equal(as.numeric(result$time_from_first[1]), 270/365.25)


  # Test that the function correctly identifies the event row for patients with events
  expect_equal(result$patno[which(result$event == 1)], n_patients)
  
  # Test with TRUE/FALSE events
  test_data$event <- as.logical(test_data$event)
  result_logical <- suppressWarnings(prep_data_km(test_data, id = "patno", date = "datum", event = "event"))
  expect_equal(sum(result_logical$event), 1)
})

test_that("prep_data_km handles errors appropriately", {
  # Create sample test data with invalid event values
  test_data_invalid <- data.frame(
    id = 1:5,
    date = as.Date("2020-01-01") + 1:5,
    event = c(0, 1, 2, 1, 0)  # Invalid value '2'
  )
  
  # Test that the function throws an error with invalid event values
  expect_error(
    prep_data_km(test_data_invalid),
    "The 'event' variable must be binary \\(either 0/1 or TRUE/FALSE\\)."
  )
  
  # Test missing dplyr package
  # Create a mock function that always returns FALSE for requireNamespace
  mock_require <- mockery::mock(FALSE, cycle = TRUE)
  
  # Use mockery to temporarily replace requireNamespace
  mockery::stub(prep_data_km, "requireNamespace", mock_require)
  
  # Test the error is thrown when dplyr is not available
  expect_error(
    prep_data_km(test_data_invalid),
    "The dplyr package is required but is not installed. Please install it first."
  )
  
  # Skip if mockery is not available
  skip_if_not_installed("mockery")
})

