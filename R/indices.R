#' HOMA-IR Calculation
#'
#' Calculates the HOMA-IR (Homeostatic Model Assessment of Insulin Resistance) based on fasting insulin and glucose values.
#' 
#' @param insulin_000 The fasting insulin value. 
#' @param glucose_000 The fasting glucose value. 
#' @param unit_insulin Character string specifying the unit for insulin. Use "SI" for pmol/l or "conventional" for µU/ml. If conventional is specified, it assumes a conversion factor of 6. Defaults to "SI".
#' @param unit_glucose Character string specifying the unit for glucose. Use "SI" for mmol/l or "conventional" for mg/dl. Defaults to "conventional"
#' @return HOMA-IR
#' @examples 
#' # Example usage
#' HOMAIR <- calculate_HOMAIR(60, 100, unit_insulin = "SI", unit_glucose = "conventional")
#' data <- data %>% mutate(HOMAIR = calculate_HOMAIR(insulin_000, glucose_000, unit_insulin = "SI", unit_glucose = "SI"))
#' @export
calculate_HOMAIR <- function(insulin_000, glucose_000, unit_insulin = "SI", unit_glucose = "conventional") {
  
  # Input checks
  if (!unit_insulin %in% c("SI", "conventional")) {
    stop("Invalid unit for insulin. Choose either 'SI' for pmol/l or 'conventional' for µU/ml.")
  }
  
  if (!unit_glucose %in% c("SI", "conventional")) {
    stop("Invalid unit for glucose. Choose either 'SI' for mmol/l or 'conventional' for mg/dl.")
  }
    
  if (unit_glucose == "conventional") {
    glucose_000 <- glucose_000 / 18.016  # Conversion from mg/dl to mmol/l
  }
   if (unit_insulin == "SI") {
        insulin_000 <- insulin_000 / 6
    }
  
  
  # HOMA-IR calculation
  return((insulin_000 * glucose_000) / 22.5)
}
calculate_HOMAIR(60, 100, unit_insulin = "SI", unit_glucose = "conventional")

#' HOMA-B Calculation
#'
#' Calculates the HOMA-B (Homeostatic Model Assessment of Beta-Cell Function) based on fasting insulin and glucose values.
#' 
#' @param insulin_000 The fasting insulin value.
#' @param glucose_000 The fasting glucose value.
#' @param unit_insulin Character string specifying the unit for insulin. Use "SI" for pmol/l or "conventional" for µU/ml. Defaults to "SI".
#' @param unit_glucose Character string specifying the unit for glucose. Use "SI" for mmol/l or "conventional" for mg/dl. Defaults to "conventional".
#' @return HOMA-B
#' @examples 
#' # Example usage
#' HOMAB <- calculate_HOMAB(80, 80, unit_insulin = "SI", unit_glucose = "conventional")
#' data <- data %>% mutate(HOMAB = calculate_HOMAB(insulin_000, glucose_000, unit_insulin = "SI", unit_glucose = "SI"))
#' @export
 <- function(insulin_000, glucose_000, unit_insulin = "SI", unit_glucose = "conventional") {
  
  # Input checks
  if (!unit_insulin %in% c("SI", "conventional")) {
    stop("Invalid unit for insulin. Choose either 'SI' for pmol/l or 'conventional' for µU/ml.")
  }
  
  if (!unit_glucose %in% c("SI", "conventional")) {
    stop("Invalid unit for glucose. Choose either 'SI' for mmol/l or 'conventional' for mg/dl.")
  }
  
  if (unit_glucose == "conventional") {
    glucose_000 <- glucose_000 / 18.016  # Conversion from mg/dl to mmol/l
  }

   if (unit_insulin == "SI") {
        insulin_000 <- insulin_000 / 6
    }
  
if (is.na(glucose_000) | is.na(insulin_000)) {
  return(NA)
} else if (glucose_000 != 3.5) {
  return((20 * insulin_000) / (glucose_000 - 3.5))
} else {
  return(NA)
}# return NA when division by 0
}

#' OGIS Calculation
#'
#' Calculates the OGIS Index for a 5-point OGTT (Oral Glucose Insulin Sensitivity).
#' 
#' @param glucose_000 The fasting glucose value. 
#' @param glucose_090 The 90 min glucose value. 
#' @param glucose_120 The 120 min glucose value. 
#' @param insulin_000 The fasting insulin value. 
#' @param insulin_090 The 90 min insulin value.
#' @param weight Weight in kg.
#' @param height Height in cm.
#' @param dose Amount of glucose in OGTT in g. Defaults to 75.
#' @param unit_insulin Character string specifying the unit for insulin. Use "SI" for pmol/l or "conventional" for µU/ml. Defaults to "SI".
#' @param unit_glucose Character string specifying the unit for glucose. Use "SI" for mmol/l or "conventional" for mg/dl. Defaults to "conventional".
#' @return OGIS
#' @examples 
#' # Example usage
#' data <- data %>% mutate(OGIS = calculate_OGIS(glucose_000, glucose_090, glucose_120, insulin_000, insulin_090, weight, height, unit_insulin = "SI", unit_glucose = "conventional"))
#' @export
#' 
calculate_OGIS <- function(glucose_000, glucose_090, glucose_120, insulin_000, insulin_090, weight, height, dose = 75, unit_insulin = "SI", unit_glucose = "conventional") {
  
  # Input checks for units
  if (!unit_insulin %in% c("SI", "conventional")) {
    stop("Invalid unit for insulin. Choose either 'SI' for pmol/l or 'conventional' for µU/ml.")
  }
  
  if (!unit_glucose %in% c("SI", "conventional")) {
    stop("Invalid unit for glucose. Choose either 'SI' for mmol/l or 'conventional' for mg/dl.")
  }
  
  # Convert insulin values if needed
  if (unit_insulin == "conventional") {
    insulin_000 <- insulin_000 * 6  # µU/ml to pmol/l
    insulin_090 <- insulin_090 * 6
  }
  
  # Convert glucose values if needed
  if (unit_glucose == "conventional") {
    glucose_000 <- glucose_000 / 18.016  # mg/dl to mmol/l
    glucose_090 <- glucose_090 / 18.016
    glucose_120 <- glucose_120 / 18.016
  }
  
  # Calculate Body Surface Area (BSA)
  BSA <- 0.1640443958298 * weight^0.515 * (0.01 * height)^0.422
  
  # Normalize dose
  NDose <- 5.551 * dose / BSA
  
  # Calculate intermediate variables
  X1 <- 792 * ((6.5 * NDose - 10000 * (glucose_120 - glucose_090) / 30) / glucose_090 + 4514 / glucose_000) / (insulin_090 - insulin_000 + 1951)
  X2 <- (0.0118 * (glucose_090 - 4.9959) + 1) * X1
  
  # Calculate OGIS
  OGIS <- (X2 + sqrt(X2^2 + 4 * 0.0118 * 173 * (glucose_090 - 4.9959) * X1)) / 2
  
  return(OGIS)
}


#' Matsuda Calculation
#'
#' Calculates the Matsuda Index for a 120 min 5-point OGTT (Oral Glucose Tolerance Test).
#' 
#' @param glucose_000 The fasting glucose value.  
#' @param glucose_030 The 30 min glucose value.  
#' @param glucose_060 The 60 min glucose value.  
#' @param glucose_090 The 90 min glucose value.  
#' @param glucose_120 The 120 min glucose value.  
#' @param insulin_000 The fasting insulin value.  
#' @param insulin_030 The 30 min insulin value.  
#' @param insulin_060 The 60 min insulin value.  
#' @param insulin_090 The 90 min insulin value.  
#' @param insulin_120 The 120 min insulin value.  
#' @param unit_insulin Character string specifying the unit for insulin. Use "SI" for pmol/l or "conventional" for µU/ml. Defaults to "SI".
#' @param unit_glucose Character string specifying the unit for glucose. Use "SI" for mmol/l or "conventional" for mg/dl. Defaults to "conventional".
#' @return Matsuda Index
#' @examples 
#' data <- data %>% mutate(Matsuda = calculate_matsuda(glucose_000, glucose_030, glucose_060, glucose_090, glucose_120, 
#'                                                      insulin_000, insulin_030, insulin_060, insulin_090, insulin_120, 
#'                                                      unit_insulin = "SI", unit_glucose = "conventional"))
#' @export
calculate_matsuda <- function(glucose_000, glucose_030, glucose_060, glucose_090, glucose_120, 
                              insulin_000, insulin_030, insulin_060, insulin_090, insulin_120, 
                              unit_insulin = "SI", unit_glucose = "conventional") {
  
  # Input checks for units
  if (!unit_insulin %in% c("SI", "conventional")) {
    stop("Invalid unit for insulin. Choose either 'SI' for pmol/l or 'conventional' for µU/ml.")
  }
  
  if (!unit_glucose %in% c("SI", "conventional")) {
    stop("Invalid unit for glucose. Choose either 'SI' for mmol/l or 'conventional' for mg/dl.")
  }
  
  # Convert insulin values if needed
  if (unit_insulin == "conventional") {
    insulin_000 <- insulin_000 * 6  # µU/ml to pmol/l
    insulin_030 <- insulin_030 * 6
    insulin_060 <- insulin_060 * 6
    insulin_090 <- insulin_090 * 6
    insulin_120 <- insulin_120 * 6
  }
  
  # Convert glucose values if needed
  if (unit_glucose == "conventional") {
    glucose_000 <- glucose_000 / 18.016  # mg/dl to mmol/l
    glucose_030 <- glucose_030 / 18.016
    glucose_060 <- glucose_060 / 18.016
    glucose_090 <- glucose_090 / 18.016
    glucose_120 <- glucose_120 / 18.016
  }
  
  # Matsuda Index calculation
  Matsuda <- 10000 / sqrt(
    (glucose_000 * 18 * insulin_000 / 6) *
      ((glucose_000 + glucose_030 * 2 + glucose_060 * 2 + glucose_090 * 2 + glucose_120) / 8 * 18) *
      ((insulin_000 + insulin_030 * 2 + insulin_060 * 2 + insulin_090 * 2 + insulin_120) / 8 / 6)
  )
  
  return(Matsuda)
}


#' FLI Calculation
#' 
#' Calculates the fatty liver index (FLI)
#' 
#' @param triglycerides Triglycerides level. 
#' @param bmi Body Mass Index (BMI).
#' @param ggt Gamma-glutamyl transferase (GGT) level. 
#' @param waist Waist circumference in cm.
#' @param unit_triglycerides Triglycerides unit. Use "conventional" for mg/dl or "SI" for mmol/l. Defaults to "conventional".
#' @param unit_ggt GT unit. Use "conventional" for IU/L or "SI" for µkat/L. Defaults to "conventional".
#' @return Fatty Liver Index (FLI)
#' @examples
#' FLI <- calculate_FLI(100, 24.2, 16, 70, unit_triglycerides = "conventional", unit_ggt = "conventional")
#' data <- data %>% mutate(FLI = calculate_FLI(triglycerides, bmi, ggt, waist, unit_triglycerides = "SI", unit_ggt = "conventional"))
#' @export
calculate_FLI <- function(triglycerides, bmi, ggt, waist, 
                          unit_triglycerides = "conventional", unit_ggt = "conventional") {

  # Input checks for units
  if (!unit_triglycerides %in% c("SI", "conventional")) {
    stop("Invalid unit for triglycerides. Choose either 'SI' for mmol/l or 'conventional' for mg/dl.")
  }
  
  if (!unit_ggt %in% c("SI", "conventional")) {
    stop("Invalid unit for GGT. Choose either 'SI' for µkat/L or 'conventional' for IU/L.")
  }
  
  # Convert triglycerides if needed
  if (unit_triglycerides == "SI") {
    triglycerides <- triglycerides * 88.57  # Conversion from mmol/l to mg/dl
  }
  
  # Convert GGT if needed
  if (unit_ggt == "SI") {
    ggt <- ggt * 60  # Conversion from µkat/L to IU/L
  }
  
  # FLI calculation
  numerator <- exp((0.953 * log(triglycerides)) + (0.139 * bmi) + (0.718 * log(ggt)) + (0.053 * waist) - 15.745)
  denominator <- 1 + numerator
  
  return((numerator / denominator) * 100)
}



#' Area under the curve (AUC) Calculation
#' 
#' Calculates the AUC between two timepoints using the trapezoid method
#' @param TP1 Value at the first timepoint 
#' @param TP2 Value at the second timepoint
#' @param Time Time interval in minutes
#' @examples 
#' data <- data %>% mutate(auc = calculate_auc(glucose_000, glucose_120, 120)
#' @export
calculate_auc <- function(TP1, TP2, Time) {
  auc <- (TP1 + TP2) * Time / 2
  return(auc)
}


#' Calculate eGFR
#' 
#' Calculates the AUC between two timepoints using the trapezoid method
#' @param creatinine Serum creatinine in mg/dl
#' @param age Age in year
#' @param sex Sex
#' @param female string of which value in sex equals female. Defaults to "female"
#' @examples
#'data <- data %>% mutate(egfr = calculate_egfr(creatinine, age, sex))
#' @export
calculate_egfr <- function(creatinine, age, sex, female = "female") {

  
  # Initialize the factor vector
  factor <- numeric(length(creatinine))
  
  factor[sex == female] <- ifelse(creatinine[sex == female] <= 0.7,
                                    144 * (creatinine[sex == female] / 0.7) ** (-0.329),
                                    144 * (creatinine[sex == female] / 0.7) ** (-1.209))
  
  factor[sex != female ] <- ifelse(creatinine[sex != female] <= 0.9,
                                  141 * (creatinine[sex != female] / 0.9) ** (-0.411),
                                  141 * (creatinine[sex != female] / 0.9) ** (-1.209))
  
  # Calculate the age factor
  age_factor <- 0.993 ** age
  
  # Calculate eGFR
  egfr <- factor * age_factor
  
  return(egfr)
}



#' FIB-4 Calculation
#'
#' Calculates the FIB-4 index, a non-invasive score for assessing liver fibrosis.
#' 
#' @param age Age of the individual in years.
#' @param ast Aspartate aminotransferase (AST / GOT) level.
#' @param alt Alanine aminotransferase (ALT / GPT) level.
#' @param platelets Platelet count in 10^9/L.
#' @param unit Unit for AST/GOT and ALT/GPT. Use "conventional" for IU/L or "SI" for µkat/L. Defaults to "conventional".
#' @return FIB-4 index
#' @examples
#' fib4 <- calculate_fib4(age = 55, ast = 45, alt = 30, platelets = 220, unit = "conventional")
#' data <- data %>% mutate(FIB4 = calculate_fib4(age, ast, alt, platelets))
#' @export
calculate_fib4 <- function(age, ast, alt, platelets, 
                           unit = "conventional") {

  # Input checks for units
  if (!unit %in% c("SI", "conventional")) {
    stop("Invalid unit. Choose either 'SI' for µkat/L or 'conventional' for IU/L.")
  }
  

  # Convert AST / ALT if needed
  if (unit == "SI") {
    alt <- alt * 60  # Conversion from µkat/L to IU/L
    ast <- ast * 60 

  }
  
  # FIB-4 calculation
  fib4 <- (age * ast) / (platelets * sqrt(alt))
  
  return(fib4)
}

