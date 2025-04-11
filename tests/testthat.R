library(testthat)
library(DiabUtils)


test_check("DiabUtils") 

# Install the package in development mode
devtools::load_all()

# Run the tests
devtools::test() 
