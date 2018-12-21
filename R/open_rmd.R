#' @title Short function to open a Rmd file
#'
#' @description
#'   This function opens a Rmd file. Please use just file names like they are
#'   named in the getStructure function
#' @param rmd.file [`character(1)`]\cr
#'   Slide name.
#' @export
openRmd = function (rmd.file) {
  cat(sprintf("Opening '%s'.", readSlide(rmd.file)))
  file.edit(readSlide(rmd.file))
}
