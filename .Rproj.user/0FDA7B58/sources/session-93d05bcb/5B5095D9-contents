#' Continuos Variable Rendering
#'
#'To be used in Table 1 package
#' @export
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c("",
                                                           "MEDIAN [IQR]" = sprintf("%s [%s-%s]", MEDIAN, Q1, Q3)#, 
                                                           #"MEAN (SD)" = sprintf("%s ± (%s)", MEAN, SD)
  ))
}
 
#' Categorical Variable Rendering
#'
#'To be used in Table 1 package
#' @export
my.render.cat <- function(x) {
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
  c("", format.pval(p, digits=3, eps = 0.001))
}
