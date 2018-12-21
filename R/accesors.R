#' @title Get package names from slideset.
#'
#' @description
#'   This function looks for packages used within the slides. However, improvement is needed for a nice List of the packages.
#'
#' @param course.list [`list`]\cr
#'   The list object containing the course structure.
#' @export
getPackagesFromSlides = function (course.list) {
  files = unlist(
    lapply(course.list, function (x) {
      lapply(x, function (y){
        if (class(y) == "character") {
          return (y)
        }
      })
    })
  )
  return(lapply(files, function (nm) {
    path = readSlide(nm)

    lines = readLines(con = path)
    return(list(
      namespaces = lines[grep(x = lines, pattern = "::")],
      libraries = lines[grep(x = lines, pattern = "library")]
    ))
  }))
}

#' @title Get directory of used slidesets.
#'
#' @description
#'   This function returns a data frame containing the path to the file location of a specific slide chunk.
#'
#' @param course_list [`list`]\cr
#'   The list object containing the course structure.
getSlidePaths = function (course_list) {
  course_list = intro_course
  a = unlist(lapply(course_list, function (x) {
    unlist(lapply(x, function (y)
    {
      if (inherits(y, "character")) {
        slide_path = readSlide(y)
        names(slide_path) = y
        return (slide_path)
      }
    }))
  }))
  slides   = vapply(X = strsplit(x = names(a), split = "[.]"), FUN = function (x) { x[1] }, FUN.VALUE = character(1L))
  sections = vapply(X = strsplit(x = names(a), split = "[.]"), FUN = function (x) { x[2] }, FUN.VALUE = character(1L))
  return (data.frame(slide = slides, section = sections, path = unname(a)))
}
