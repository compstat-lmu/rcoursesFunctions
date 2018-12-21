#' @title Get the path to a specific chapter
#'
#' @description
#'   The function `readSlide` gives the path of a specific chapter file.
#'   This avoids long paths for the child parameter at the Rmd files.
#'
#' @param slide.name [`character(1)`]\cr
#'   The name of a specific file. To see which files are existing use
#'   `getStructure`.
#' @param path [`character(1)`]\cr
#'   The working directory of the rcourses repo. Please notice, that every path
#'   containing `/rcourses` will work.
#' @param file [`logical(1)`]\cr
#'   Specify if the returned path should contain the file or just the directory
#'   which contains the file.
#' @return [`character(1)`]\cr
#'   `readSlide` returns a string containing the path of the file.
#' @examples
#'   (test = readSlide("about_r"))
#'   file.exists(test)
#' @export
readSlide = function (slide.name, path = getwd(), file = TRUE) {

  # Read the structure of the chapters or exercises:
  struct = getStructure()

  if (! slide.name %in% struct[["files"]]) {
    stop("\"slides_", slide.name, ".Rmd\" is not available.")
  }

  struct$slide = paste0("slides_", struct$files)

  if (! slide.name %in% struct$files) {
    stop("There is no file slide_", slide.name, ".Rmd")
  }
  # struct[, slide := paste0("slides_", files)]
  struct$files = NULL

  # Path to the rcourses rep:
  path = paste0(strsplit(path, "/rcourses")[[1]][1], "/rcourses/chapters/")

  file.idx = which(struct$slide == paste0("slides_", slide.name))

  # Very ugly but saver than everything else I tried:
  file.dir = c(
    as.character(struct[file.idx, 1]),
    as.character(struct[file.idx, 2]),
    as.character(struct[file.idx, 3])
  )

  if (file) {
    # Path to the slide:
    path.to.slide = paste0(path,
      paste(file.dir, collapse = "/"),
      ".Rmd")
  } else {
    # Path to the slide:
    path.to.slide = paste0(
      path,
      paste(file.dir[-length(file.dir)], collapse = "/")
    )
  }
  return(path.to.slide)
}
