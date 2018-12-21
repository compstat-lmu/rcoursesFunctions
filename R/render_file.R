#' @title Render just one slide set.
#'
#' @description
#'   This function creates and render just one slide set.
#'
#' @param slide.name [`character(1)`]\cr
#'   Name of the slide as given from the getStructure function.
#' @param open.pdf [`character(1)`]\cr
#'   A character vector telling whether the resulting PDF file should be opened. Default = FALSE.
#' @param ...
#'   Passed to `createCourse()`
#' @return [`character`]\cr
#'   A character vector containing the lines of the final Rmd file for the
#'   specific file named in 'slide.name'.
#' @export
renderFile = function (slide.name, open.pdf = FALSE, ...) {
  return(createCourse(
    title = slide.name
    , course = "single_files"
    , file.name = paste0("single_rmd_", slide.name)
    , yaml.subtitle = ""
    , subtitles = NA
    , author = ""
    , course.date = ""
    , keep.source = FALSE
    , year = "slide"
    , month = slide.name
    , render = TRUE
    , course.list = list(list(slide.name))
    , open.pdf = open.pdf
    , ...
  ))
}
