# DiabUtils

<img src="logo/imgfile.png" alt="Logo for DiabUtils Package displaying sugarcubes" height="300">

DiabUtils is a utility package for diabetes data analysis. It includes functions for index calculation and visualization of (diabetes-related) datasets.

## Installation

```r
# Make sure that the devtools library is loaded
library(devtools)

# Install DiabUtils from GitHub
install_github("SportyScientist/DiabUtils")
```
## Functions
All functions are designed to work with dplyr / the tidyverse.
### Index Calculations
- calculate_OGIS()
- calculate_HOMAIR()
- calculate_HOMAB()
- calculate_matsuda()
- calculate_FLI()
- calculate_auc

### Classifications
- calculate_glycat()

### Data Preparation
- prep_data_km(): Preps data for input into ggsurvival package

### Table1 
Used in conjunction with the Table1 package (https://cran.r-project.org/web/packages/table1/vignettes/table1-examples.html)
- my.render.cont()
- my.render.cat()
- pvalue()