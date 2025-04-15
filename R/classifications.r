#' Determine glycemic categories
#'
#' As Defined by:
#' NGT: Fasting glucose < 100, 120min glucose < 140, hba1c < 5.7 (ADA)
#' iIFG: Fasting glucose elevated
#' iIGT: 120min glucose elevated
#' IFG+IGT: Fasting and 120min glucose elevated
#' DIA: Fasting glucose above 126 OR 120min glucose above 200 OR HBA1C above 6.5
#' @param glucose_000 The fasting glucose value
#' @param glucose_120 The 120min glucose value
#' @param hba1c HbA1c value. Not required when system = WHO.
#' @param use_hba1c_pre Logical (TRUE/FALSE). Only relevant when system = ADA. Whether to use HbA1c for classification of prediabetes. Defaults to TRUE.
#' @param use_hba1c_dia Logical (TRUE/FALSE). Only relevant when system = WHO Whether to use HbA1c for classification of diabetes. Defaults to FALSE.
#' @param system ADA or WHO. Defaults to ADA. IMPORTANT: Fasting glucose threshold for prediabetes is 100 mg/dL for ADA and 110 mg/dL for WHO! 
#' @param details Logical (TRUE/FALSE). Wether the function should return a detailed differentiation of prediabetes or just "PRE". Defaults to FALSE
#' @param unit_glucose Unit of the glucose values, can be SI (mmol/l) or conventional (mg/dl). Defaults to conventional.
#' @param unit_hba1c Unit of the hba1c values, can be SI (mmol/mol) or perc (%). Defaults to perc.
#'
#' @return Glycemic Category
#' @examples
#' classify_glycemia(90, 160, 6.9, system = "ADA", details = TRUE)
#' @export
classify_glycemia <- function(glucose_000, glucose_120, hba1c = NULL, use_hba1c_pre = TRUE, use_hba1c_dia = FALSE, system = "ADA",
                              details = FALSE,
                              unit_glucose = "conventional", unit_hba1c = "perc") {
  
  validate_value <- function(value) {
    if (is.na(value)) {
      return(FALSE)
    }
    return(!is.numeric(value) || value < 0)
  }

  # Input checks
  if (any(sapply(glucose_000, validate_value))) {
    stop("Error: 'glucose_000' must be a positive numeric vector.")
  }
  if (any(sapply(glucose_120, validate_value))) {
    stop("Error: 'glucose_120' must be a positive numeric vector.")
  }
  if (!system %in% c("ADA", "WHO")) {
    stop("Error: 'system' must be either 'ADA' or 'WHO'.")
  }
  if (system == "ADA" && is.null(hba1c)) {
    stop("Error: 'hba1c' values are required when 'system' is set to 'ADA'.")
  }
  if (any(sapply(hba1c, validate_value))) {
    stop("Error: 'hba1c' must be a positive numeric vector or NULL.")
  }
  if (!is.logical(details)) {
    stop("Error: 'details' must be a logical value (TRUE or FALSE).")
  }
  if (!unit_glucose %in% c("SI", "conventional")) {
    stop("Error: 'unit_glucose' must be either 'SI' or 'conventional'.")
  }
  if (!unit_hba1c %in% c("perc", "SI")) {
    stop("Error: 'unit_hba1c' must be either 'perc' or 'SI'.")
  }
  
  if (!is.logical(use_hba1c_pre)) {
    stop("Error: 'use_hba1c_pre' must be a logical value (TRUE or FALSE).")
  }
  if (!is.logical(use_hba1c_dia)) {
    stop("Error: 'use_hba1c_dia' must be a logical value (TRUE or FALSE).")
  }

  # Convert units if necessary
  if (unit_glucose == "SI") {
    glucose_000 <- glucose_000 * 18.016
    glucose_120 <- glucose_120 * 18.016
  }
  if (unit_hba1c == "SI" && !is.null(hba1c)) {
    hba1c <- (hba1c * 0.0915) + 2.15
  }

  # Function to classify individual cases
  classify_single <- function(glucose_000, glucose_120, hba1c) {
    result <- "Unknown"
    
    # ADA Classification
    if (system == "ADA") {

      if ((!is.na(hba1c) && hba1c >= 6.5) ||
          (!is.na(glucose_000) && glucose_000 >= 126) ||
          (!is.na(glucose_120) && glucose_120 >= 200)) {
        result <- "DIA"
        
      }  else if (is.na(glucose_000) || is.na(glucose_120) || (use_hba1c_pre && is.na(hba1c))) {
        result <- NA
      } else {
        # Check for prediabetes criteria
        prediabetes_criteria <- (glucose_000 >= 100 && glucose_000 < 126) ||
                               (glucose_120 >= 140 && glucose_120 < 200)
        
        # Add HbA1c criteria for prediabetes only if use_hba1c_pre is TRUE
        if (use_hba1c_pre && !is.na(hba1c) && hba1c >= 5.7 && hba1c < 6.5) {
          prediabetes_criteria <- TRUE
        }
        
        if (prediabetes_criteria) {
          if (details) {
            if (glucose_000 >= 100 && glucose_000 < 126 && glucose_120 < 140) {
              result <- "iIFG"
            } else if (glucose_120 >= 140 && glucose_120 < 200 && glucose_000 < 100) {
              result <- "iIGT"
            } else if (glucose_000 >= 100 && glucose_000 < 126 && glucose_120 >= 140 && glucose_120 < 200) {
              result <- "IFG+IGT"
            } else if (use_hba1c_pre && !is.na(hba1c) && hba1c >= 5.7 && hba1c < 6.5 && 
                      glucose_000 < 100 && glucose_120 < 140) {
              result <- "HBA1C"
            } else {
              result <- "PRE"
            }
          } else {
            result <- "PRE"
          }
        } else {
          result <- "NGT"
        }
      }
    }
    
    # WHO Classification
    if (system == "WHO") {
      # Check for diabetes criteria
      diabetes_criteria <- (!is.na(glucose_000) && glucose_000 >= 126) || 
                           (!is.na(glucose_120) && glucose_120 >= 200)
      
      # Add HbA1c criteria for diabetes only if use_hba1c_dia is TRUE
      if (use_hba1c_dia && !is.null(hba1c) && !is.na(hba1c) && hba1c >= 6.5) {
        diabetes_criteria <- TRUE
      }
      
      if (diabetes_criteria) {
        result <- "DIA"
      } else if (is.na(glucose_000) || is.na(glucose_120)) {
        result <- NA
      } else if ((glucose_000 >= 110 && glucose_000 < 126) ||
                 (glucose_120 >= 140 && glucose_120 < 200)) {
        if (details) {
          if (glucose_000 >= 110 && glucose_000 < 126 && glucose_120 < 140) {
            result <- "iIFG"
          } else if (glucose_120 >= 140 && glucose_120 < 200 && glucose_000 < 110) {
            result <- "iIGT"
          } else if (glucose_000 >= 110 && glucose_000 < 126 && glucose_120 >= 140 && glucose_120 < 200) {
            result <- "IFG+IGT"
          }
        } else {
          result <- "PRE"
        }
      } else {
        result <- "NGT"
      }
    }
    
    return(result)
  }

  # Handle both single values and vectors
  # If any input is a vector of length > 1, use mapply for vectorized operation
  if (length(glucose_000) > 1 || length(glucose_120) > 1 || (!is.null(hba1c) && length(hba1c) > 1)) {
    # For WHO classification where hba1c might be NULL
    if (is.null(hba1c)) {
      # Create a dummy hba1c vector of NAs with the same length as the longest input
      max_length <- max(length(glucose_000), length(glucose_120))
      dummy_hba1c <- rep(NA, max_length)
      results <- mapply(classify_single, glucose_000, glucose_120, dummy_hba1c, SIMPLIFY = TRUE)
    } else {
      results <- mapply(classify_single, glucose_000, glucose_120, hba1c, SIMPLIFY = TRUE)
    }
  } else {
    # For single values, just call the function directly
    results <- classify_single(glucose_000, glucose_120, hba1c)
  }
  
  return(results)
}


