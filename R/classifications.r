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
#' @param system ADA or WHO. Defaults to ADA
#' @param details Logical (TRUE/FALSE). Wether the function should return a detailed differentiation of prediabetes or just "PRE". Defaults to FALSE
#' @param unit_glucose Unit of the glucose values, can be SI (mmol/l) or conventional (mg/dl). Defaults to conventional.
#' @param unit_hba1c Unit of the hba1c values, can be SI (mmol/mol) or perc (%). Defaults to perc.
#'
#' @return Glycemic Category
#' @examples
#' classify_glycemia(90, 160, 6.9, system = "ADA", details = TRUE)
#' @export
classify_glycemia <- function(glucose_000, glucose_120, hba1c = NULL, system = "ADA",
                              details = FALSE,
                              unit_glucose = "conventional", unit_hba1c = "perc") {
  validate_value <- function(value) {
    if (is.na(value)) {
      return(FALSE)
    }
    return(!is.numeric(value) || value < 0)
  }

  # Input checks
  if (validate_value(glucose_000)) {
    stop("Error: 'glucose_000' must be a positive numeric value.")
  }
  if (validate_value(glucose_120)) {
    stop("Error: 'glucose_120' must be a positive numeric value.")
  }
  if (!system %in% c("ADA", "WHO")) {
    stop("Error: 'system' must be either 'ADA' or 'WHO'.")
  }
  if (system == "ADA" && is.null(hba1c)) {
    stop("Error: 'hba1c' value is required when 'system' is set to 'ADA'.")
  }
  if (!is.null(hba1c) && (!is.numeric(hba1c) || hba1c <= 0)) {
    stop("Error: 'hba1c' must be a positive numeric value or NULL.")
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

  # Convert units if necessary
  if (unit_glucose == "SI") {
    glucose_000 <- glucose_000 * 18.016
    glucose_120 <- glucose_120 * 18.016
  }

  if (unit_hba1c == "SI" && !is.null(hba1c)) {
    hba1c <- (hba1c * 0.0915) + 2.15
  }

  # Initialize result
  result <- "Unknown"

  # ADA Classification
  if (system == "ADA") {
    if ((!is.na(hba1c) && hba1c >= 6.5) ||
      (!is.na(glucose_000) && glucose_000 >= 126) ||
      (!is.na(glucose_120) && glucose_120 >= 200)) {
      result <- "DIA"
    } else if (is.na(hba1c) || is.na(glucose_000) || is.na(glucose_120)) {
      result <- NA
    } else if ((glucose_000 >= 100 && glucose_000 < 126) ||
      (glucose_120 >= 140 && glucose_120 < 200) ||
      (hba1c >= 5.7 && hba1c < 6.5)) {
      if (details) {
        if (glucose_000 >= 100 && glucose_000 < 126 &&
          glucose_120 < 140) {
          result <- "iIFG"
        } else if (glucose_120 >= 140 && glucose_120 < 200 &&
          glucose_000 < 100) {
          result <- "iIGT"
        } else if (glucose_000 >= 100 && glucose_000 < 126 &&
          glucose_120 >= 140 && glucose_120 < 200) {
          result <- "IFG+IGT"
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

  # WHO Classification
  if (system == "WHO") {
    if ((!is.na(glucose_000) && glucose_000 >= 126) ||
      (!is.na(glucose_120) && glucose_120 >= 200)) {
      result <- "DIA"
    } else if (is.na(glucose_000) || is.na(glucose_120)) {
      result <- NA
    } else if ((glucose_000 >= 110 && glucose_000 < 126) ||
      (glucose_120 >= 140 && glucose_120 < 200)) {
      if (details) {
        if (glucose_000 >= 110 && glucose_000 < 126 &&
          glucose_120 < 140) {
          result <- "iIFG"
        } else if (glucose_120 >= 140 && glucose_120 < 200 &&
          glucose_000 < 110) {
          result <- "iIGT"
        } else if (glucose_000 >= 110 && glucose_000 < 126 &&
          glucose_120 >= 140 && glucose_120 < 200) {
          result <- "IFG+IGT"
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

  return(result)
}
