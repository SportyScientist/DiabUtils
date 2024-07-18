#' Determine glycemic categories
#'
#' As Defined by: 
#' NGT -> Fasting glucose < 100, 120min glucose < 140
#' IFG: Fasting glucose elevated
#' IGT: 120min glucose elevated
#' IFG+IGT: Fasting ans 120min glucose elevated
#' DIA: Fasting glucose above 126 OR 120min glucose above 200 OR HBA1C above 6.5
#' @param glucose_000 The fasting glucose value 
#' @param glucose_120 The 120min glucose value 
#' @param hba1c HbA1c value in %
#' @param units Unit of the glucose, can me mgdl or mmoll
#' @return Glycemic Category
#' @examples 
#' result <- calculate_glycat(90, 150, 5.8, units = "mgdl");
#' data <- data %>% mutate(category = calculate_glycat(glucose_000, glucose_120, hab1c))
#' @export
calculate_glycat <- function (glucose_000, glucose_120, hba1c, units = "mgdl") 
{
  # Check if input variables are numeric
  if (!is.numeric(glucose_000) || !is.numeric(glucose_120) || !is.numeric(hba1c)) {
    stop("All input variables must be numeric.")
  }
  
  # Check if units is either "mgdl" or "mmoll"
  if (!units %in% c("mgdl", "mmoll")) {
    stop("Units must be either 'mgdl' or 'mmoll'.")
  }
  
  # Convert mmol/l to mg/dl if needed
  if (units == "mmoll") {
    glucose_000 <- glucose_000 * 18.0182
    glucose_120 <- glucose_120 * 18.0182
  }
  
  # Calculate the glycemic category
  output <- case_when(
    glucose_000 >= 126 | glucose_120 >= 200 | hba1c >= 6.5 ~ "DIA", 
    glucose_120 >= 140 & glucose_000 >= 100 ~ "IFG+IGT", 
    glucose_120 >= 140 ~ "IGT", 
    glucose_000 >= 100 ~ "IFG", 
    glucose_000 < 100 & glucose_120 < 140 ~ "NGT", 
    is.na(glucose_000) & is.na(glucose_120) ~ "", 
    TRUE ~ ""
  )
  
  return(ifelse(output == "", NA, output))
}
