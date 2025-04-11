#' Continuous Variable Rendering for Table 1
#'
#' A render function for continuous variables to be used with the table1 package.
#' This function formats continuous variables with customizable summary statistics.
#'
#' @param x A vector of numeric values
#' @param summary_type Type of summary to display: "median" (median and IQR), "mean" (mean and SD), or "both"
#' @param digits Number of decimal places for rounding
#' @return A character vector with formatted summary statistics
#' @examples
#' \dontrun{
#'   library(table1)
#'   table1(~ age + weight | group, data = mydata, 
#'          render.continuous = function(x) render_cont(x, "median", 2))
#' }
#' @export
render_cont <- function(x, summary_type = c("median", "mean", "both"), digits = 1) {
  summary_type <- match.arg(summary_type)
  
  # Calculate statistics
  stats <- stats.default(x)
  
  # Round the statistics to the specified number of decimal places
  stats$MEAN <- round(as.numeric(stats$MEAN), digits)
  stats$SD <- round(as.numeric(stats$SD), digits)
  stats$MEDIAN <- round(as.numeric(stats$MEDIAN), digits)
  stats$Q1 <- round(as.numeric(stats$Q1), digits)
  stats$Q3 <- round(as.numeric(stats$Q3), digits)
  
  # Format output based on selected summary type
  switch(summary_type,
         "median" = c("",
                      "MEDIAN [IQR]" = sprintf("%s [%s-%s]", stats$MEDIAN, stats$Q1, stats$Q3)),
         "mean" = c("",
                    "MEAN (SD)" = sprintf("%s (%s)", stats$MEAN, stats$SD)),
         "both" = c("",
                    "MEDIAN [IQR]" = sprintf("%s [%s-%s]", stats$MEDIAN, stats$Q1, stats$Q3),
                    "MEAN (SD)" = sprintf("%s (%s)", stats$MEAN, stats$SD))
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
#' To be used in Table 1 package
#' Excludes "overall" column so that the p-value doesn't get calculated on that. 
#' Performs appropriate statistical tests based on data type and number of groups:
#' - For numeric data: Wilcoxon/Kruskal-Wallis for non-parametric tests
#' - For categorical data: chi-square test
#' 
#' @param x A list of vectors to compare
#' @param ... Additional arguments
#' @examples
#' \dontrun{
#'   library(table1)
#'   table1(~ age + weight | group, data = mydata, 
#'          extra.col = list(pval = pvalue))
#' }
#' @export
pvalue <- function(x, ...) {
  x <- x[names(x) != "overall"]
  
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  df <- as.data.frame(cbind(y,g))
  NA_count <- c()
  print(names(x))
  
  for(i in 1:length(names(x))){
    NA_count[i] <- all((is.na(df$y[df$g == i])))
  }
  
  if(sum(!NA_count) == 1){
    p <- NA
  }
  else if(sum(!NA_count) == 2){
    if (is.numeric(y)) {
      # For numeric data, perform Wilcoxon rank-sum test
      p <- wilcox.test(y ~ g)$p.value
    } else {
      # For categorical variables, perform a chi-squared test of independence
      p <- chisq.test(table(y, g))$p.value
    }
  }
  else if(sum(!NA_count) > 2){
    if (is.numeric(y)) {
      # For numeric data, perform Kruskal-Wallis test
      p <- kruskal.test(y ~ g)$p.value
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



#' Test Normality of Data
#'
#' This function tests whether a numeric vector follows a normal distribution
#' and recommends an appropriate statistical test based on the result.
#'
#' @param x A numeric vector to test for normality.
#'
#' @return A character string indicating the recommended statistical test:
#'   "normal" if the data appears normally distributed (Shapiro-Wilk p > 0.05),
#'   or "non-normal" if the data does not appear normally distributed.
#'
#' @examples
#' # Test normally distributed data
#' normal_data <- rnorm(100)
#' test_normality(normal_data)
#'
#' # Test non-normally distributed data
#' skewed_data <- rexp(100)
#' test_normality(skewed_data)
#'
#' @export
test_normality <- function(x) {
  if (length(x) < 3) {
    stop("Sample size must be at least 3 for normality testing")
  }
  
  shapiro_p <- shapiro.test(x)$p.value
  
  if (shapiro_p > 0.05) {
    return("normal")
  } else {
    return("non-normal")
  }
}
