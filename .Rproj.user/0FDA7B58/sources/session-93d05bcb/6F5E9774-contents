#' prep_data_km
#'
#' Function to bring longitudinal data into the appropriate format for survival analysis.
#' @param data Dataframe in long format with one row per timepoint.
#' @param date Name of the variable holding the measurement date. Defaults to "date".
#' @param id Name of the variable holding the patient ID. Defaults to "id".
#' @param event Name of the variable holding the event. Needs to be binary (0-1 or TRUE-FALSE). Defaults to "event".
#' @return Dataframe with one row per id. 
#' @examples 
#' test_km <- prep_data_km(data, id = "patno", date = "datum")
#' @export
prep_data_km <- function(data, date = "date", id = "id", event = "event"){
  
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package is required but is not installed. Please install it first.")
  }
  
  if (!all(data[, event] %in% c(0, 1, NA)) && !all(data[, event] %in% c(TRUE, FALSE, NA))) {
    stop("The 'event' variable must be binary (either 0/1 or TRUE/FALSE).")
  }
  
  # Convert date to Date class
  data[[date]] <- as.Date(data[[date]])
    # Calculate the time from the first date to each of the other dates
  data_new <- data %>%
    filter(!is.na(!!as.name(event))) %>% 
    arrange(!!as.name(id), !!as.name(date)) %>%
    group_by(!!as.name(id)) %>%
    mutate(time_from_first = (!!as.name(date) - first(!!as.name(date))) / 365.25) %>%
    ungroup()
  
  # Keep only the first row where event is TRUE for each patient, or the last row if event is never TRUE
  data_new_2 <- data_new %>%
    arrange(!!as.name(id), !!as.name(date)) %>%
    group_by(!!as.name(id)) %>%
    slice(if (any(!!as.name(event) == TRUE )) which.max(!!as.name(event)) else n()) %>%
    ungroup()
  
 return(data_new_2)
}
