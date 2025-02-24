#' Continuos Variable Rendering
#'
#'To be used in Table 1 package
#' @export
render_cont <- function(x, summary_type = c("median", "mean", "both"), n_signif) {
  summary_type <- match.arg(summary_type)
  
  # Calculate statistics without rounding yet
  stats <- stats.default(x)
  
  # Define a custom rounding function with formatC to ensure trailing zeros
  custom_round <- function(value) formatC(value, digits = n_signif, format = "f", flag = "0")
  
  # Apply custom rounding to each statistic
  stats <- lapply(stats, custom_round)
  
  # Format output based on selected summary type
  switch(summary_type,
         "median" = c("",
                      "MEDIAN [IQR]" = sprintf("%s [%s-%s]", stats$MEDIAN, stats$Q1, stats$Q3)),
         "mean" = c("",
                    "MEAN (SD)" = sprintf("%s ± (%s)", stats$MEAN, stats$SD)),
         "both" = c("",
                    "MEDIAN [IQR]" = sprintf("%s [%s-%s]", stats$MEDIAN, stats$Q1, stats$Q3),
                    "MEAN (SD)" = sprintf("%s ± (%s)", stats$MEAN, stats$SD))
  )
}


#' Categorical Variable Rendering
#'
#'To be used in Table 1 package
#' @export
render_cat <- function(x) {
c("", sapply(stats.default(x), function(y) with(y,
                                                sprintf("%d (%0.0f %%)", FREQ, PCT)
)))
}


#' Pvalue Calculation 
#'
#'To be used in Table 1 package
#'Excludes "overall" column so that the p-value doesn't get calculated on that. Performs t-test for 2 variables, and anova for 3+ variable, chi square for categorical variables.
#' @export
pvalue <- function(x, ...) {
  x <- x[names(x) != "overall"]
  
  # Construct vectors of data y, and groups (strata) g
  
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  df <- as.data.frame(cbind(y,g))
  NA_count <- c()
  
  for(i in 1:length(names(x))){
    
    
    NA_count[i] <- all((is.na(df$y[df$g == i])))
    
  }
  
  if(sum(!NA_count) == 1){
    p <- NA
  }
  
  else if(sum(!NA_count) == 2){
    if (is.numeric(y)) {
      # For numeric variables, perform t-test
      p <- t.test(y ~ g)$p.value
      
      
    } else {
      # For categorical variables, perform a chi-squared test of independence
      p <- chisq.test(table(y, g))$p.value
    }
    
  }
  else if(sum(!NA_count) > 2){
    if (is.numeric(y)) {
      # For numeric variables, perform an anova
      SS <- aov(y ~ g)
      p <- summary(SS)[[1]][[5]][1]
      
      
    } else {
      # For categorical variables, perform a chi-squared test of independence
      p <- chisq.test(table(y, g))$p.value
    }
    
  }
  p_formatted <- ifelse(p < 0.0001, "< 0.0001", formatC(p, format = "f", digits = 2))
  c("", p_formatted)

}

#' Remove Outliers
#'
#' @param data A numeric vector or column of a data frame to process.
#' @param method A character string specifying the method to detect outliers. Options are `"iqr"` (interquartile range) or `"zscore"`.
#' @param threshold A numeric threshold for the `zscore` method (default is 3) or IQR multiplier (default is 1.5 for `"iqr"`).
#' 
#' @return A numeric vector with outliers removed.
#' @examples
#' # Remove outliers using IQR method
#' remove_outliers(c(1, 2, 3, 100), method = "iqr")
#'
#' # Remove outliers using z-score method
#' remove_outliers(c(1, 2, 3, 100), method = "zscore", threshold = 2)
#'
#' @export
#' 
remove_outliers <- function(data, method = "iqr", threshold = 1.5) {
  if (!is.numeric(data)) stop("Data must be numeric.")
  
  if (method == "iqr") {
    Q1 <- quantile(data, 0.25, na.rm = TRUE)
    Q3 <- quantile(data, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower_bound <- Q1 - threshold * IQR
    upper_bound <- Q3 + threshold * IQR
    data <- data[data >= lower_bound & data <= upper_bound]
  } else if (method == "zscore") {
    z_scores <- scale(data)
    data <- data[abs(z_scores) <= threshold]
  } else {
    stop("Invalid method. Choose 'iqr' or 'zscore'.")
  }
  
  return(data)
}
