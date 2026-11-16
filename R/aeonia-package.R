#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib aeonia, .registration = TRUE
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
#' @importFrom grDevices cairo_pdf dev.off png
#' @importFrom stats rlnorm rnorm sd
#' @importFrom utils head tail
NULL

# Package data objects used inside functions:
utils::globalVariables(c("an_dev_times", "an_environ", "an_populations",
                         "an_wasp_attack"))
