require(devtools)
require(testthat)
require(R.rsp)

options(error = NULL)

load_all()

roxygen2::roxygenize()

build_vignettes()

require(renv)
devtools::install("C:\\Users\\mthalmann\\Documents\\R-packages")
devtools::install("C:\\Users\\Mirko\\Documents\\programming\\R\\rutils")

