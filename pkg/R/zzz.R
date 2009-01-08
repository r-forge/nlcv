.onAttach <- function(libname, pkgname){
  options(error = NULL)
  message(paste("\nnlcv version ", packageDescription("nlcv")$Version, 
          "\n", sep = ""))
}
