require(devtools)
require(testthat)
require(R.rsp)

options(error = NULL)

load_all()

roxygen2::roxygenize()

build_vignettes()
