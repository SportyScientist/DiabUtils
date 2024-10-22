#' HOMA IR Calculation
#'
#' Calculates the HOMA-IR
#' @param insulin_000 The fasting insulin value in pmol/l
#' @param glucose_000 The fasting glucose value in mmol/l
#' @return HOMAR-IR
#' @examples 
#' HOMAIR <- HOMAIR(40, 80);
#' data <- data %>% mutate(HOMAIR = calculate_HOMAIR(insulin_000,glucose_000))
#' @export
calculate_HOMAIR <- function(insulin_000, glucose_000) {
  return((insulin_000 / 7) * (glucose_000 / 22.5))
}


#' HOMA-B Calculation
#'
#' Calculates the HOMA-B
#' @param insulin_000 The fasting insulin value in pmol/l
#' @param glucose_000 The fasting glucose value in mmol/l
#' @return HOMAR-IR
#' @examples 
#' HOMAB <- HOMAB(40, 80);
#' data <- data %>% mutate(HOMAB = calculate_HOMAB(insulin_000, glucose_000))
#' @export
calculate_HOMAB <- function(insulin_000, glucose_000) {
  return(20 * (insulin_000 / 7) / (glucose_000 - 3.5))
}



#' OGIS Calculation
#'
#' Calculates the OGIS Index for a 5 point OGTT
#' @param glucose_000 The fasting glucose value in mmol/l
#' @param glucose_090 The 90min glucose value in mmol/l
#' @param glucose_120 The 120min glucose value in mmol/l
#' @param insulin_000 The fasting insulin value in pmol/l
#' @param insulin_090 The 90min insulin value in pmol/l
#' @param weight Weight in kg
#' @param height Height in cm
#' @param dose Amount of glucose in OGTT in g, defaults to 75
#' @return OGIS
#' @examples 
#' data <- data %>% mutate(OGIS = calculate_OGIS(glucose_000, glucose_090, glucose_120, insulin_000, insulin_090, weight, height))
#' @export
calculate_OGIS <- function(glucose_000, glucose_090, glucose_120, insulin_000, insulin_090, weight, height, dose = 75) {

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
#' Calculates the Matsuda Index for a 5 point OGTT
#' @param glucose_000 The fasting glucose value in mmol/l
#' @param glucose_030 The fasting glucose value in mmol/l
#' @param glucose_060 The fasting glucose value in mmol/l
#' @param glucose_090 The 90min glucose value in mmol/l
#' @param glucose_120 The 120min glucose value in mmol/l
#' @param insulin_000 The fasting insulin value in pmol/l
#' @param insulin_030 The fasting insulin value in pmol/l
#' @param insulin_060 The fasting insulin value in pmol/l
#' @param insulin_090 The fasting insulin value in pmol/l
#' @param insulin_120 The 90min insulin value in pmol/l
#' @return Matsuda
#' @examples 
#' data <- data %>% mutate(Matsuda = calculate_matsuda(glucose_000, glucose_030, glucose_060, glucose_090, glucose_120, insulin_000, insulin_030, insulin_060, insulin_090, insulin_120))
#' @export
calculate_matsuda <- function(glucose_000, glucose_030, glucose_060, glucose_090, glucose_120, 
                              insulin_000, insulin_030, insulin_060, insulin_090, insulin_120) {
return (10000 / sqrt(
  (glucose_000 * 18 * insulin_000 / 6) *
    ((glucose_000 + glucose_030 * 2 + glucose_060 * 2 + glucose_090 * 2 + glucose_120) / 8 * 18) *
    ((insulin_000 + insulin_030 * 2 + insulin_060 * 2 + insulin_090 * 2 + insulin_120) / 8 / 6)
))
}

#' FLI Calculation
#' 
#' Calculates the fatty liver index (FLI)
#' @param triglycerides Triglycerides in mg/dl
#' @param bmi BMI
#' @param ggt GGT levels in IU/L
#' @param waist Waist in cm 
#' @return FLI
#' @examples
#' FLI <- calcualte_FLI(110, 35, 27, 85)
#' data <- data %>% mutate(calculate_FLI, triglycerides, bmi, ggt, waist)
#' @export
calculate_FLI <- function(triglycerides, bmi, ggt, waist) {

        numerator <- exp((0.953 * log(triglycerides)) + (0.139 * bmi) + (0.718 * log(ggt)) + (0.053 * waist) - 15.745)
        denominator <- 1 + numerator
        
        (numerator / denominator) * 100
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




