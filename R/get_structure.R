#' @title Get the structure of all existing chapters
#'
#' @description
#'   The function `getStructure` reads in the structure of all existing
#'   Rmd chapters/exercises in the chapters or exercises directory. This file
#'   is also needed in the `setup.Rmd` file. In the most cases this
#'   function is used to get the specific directory a file lies in. Therefore
#'   the chapter names have to be unique!
#'
#'   The generation of the structure is done automatically. This is very handy
#'   if someone copies Rmd files from one chapter or course to another.
#'
#'   The `files` column in the resulting data.table is also the name one
#'   can use to create a course list and create a course Rmd file with
#'   `createCourse`.
#'
#' @param type [`character(1)`]\cr
#'   The first subdirectory of the files. Have to be `chapters` or
#'   `exercises`.
#' @param path [`character(1)`]\cr
#'   The working directory of the rcourses repo. Please notice, that every path
#'   containing `/rcourses` will work.
#' @return [`data.table`]\cr
#'   `getStructure` returns a `data.table` with all the existing
#'   chapter or exercise files. The data table contains the following variables:
#'     - course:   Course which contains the file
#'     - chapters: Chapter which contains the file
#'     - files:    Name of the files
#' @export
getStructure = function (type = "chapters", path = getwd()) {
  # Split the path at "/rcourses" and paste "/rcourses" to the first element of
  # the splits:
  path = paste0(strsplit(path, "/rcourses")[[1]][1], "/rcourses")

  # Check if a type is given:
  if (! type %in% c("chapters", "exercises")) {
    stop("Please specify the type with 'chapters' or 'exercises'!")
  }

  # Paste the type to the path:
  path = paste0(path, "/", type)

  # Get the courses in the "path/type" directory:
  courses = list.dirs(path = path, full.names = FALSE, recursive = FALSE)

  # Declare empty lists for the chapters and the files:
  chapter = list()
  slides  = list()

  # the three objects "courses", "chapter" and "slides" are the main files to
  # create the structure.

  # Loop over all courses to get all available slides which can be used by
  # createCourse():
  for (i in seq_along(courses)) {
    #   1. For each course read the chapters and write them into the chapter
    #      list (for basis this could be intro_short, deskriptiv and inference).
    chapter[[i]] = list.dirs(
      path = paste0(path, "/", courses[i]),
      full.names = FALSE,
      recursive  = FALSE
    )

    #   2. Declare for each course a new list element within the slides list.
    #      For instance, ml is one list element which then gets another list
    #      of all available files.
    slides[[i]] = list()

    #   3. Loop over all the chapters of one course:
    for (j in seq_along(chapter[[i]])) {

      # Get all available slides of chapter i:
      temp.files = list.files(
        path    = paste0(path, "/", courses[i], "/", chapter[[i]][j]),
        pattern = ".Rmd"
      )

      if (length(temp.files) > 0) {
        # Just keep the chapter which contains "slides_":
        temp.files = temp.files[grepl(x = temp.files, pattern = "slides_")]

        slides[[i]][[j]] = temp.files
      }
    }
  }

  # Declare data.table:
  struct = data.frame(
    matrix(
      data = NA_character_,
      nrow = 0L,
      ncol = 3L
    ),
    stringsAsFactors = FALSE
  )

  colnames(struct) = c("course", "chapters", "files")

  # Loop which fills the structure with all the courses, chapters and files:

  for (i in seq_along(courses)) {
    # lgts conatins how much files exists for the subchapters of course i:
    lgts = unlist(lapply(slides[[i]], length))

    # Loop over the subchapters:
    for (j in seq_along(lgts)) {

      if (length(slides[[i]][[j]]) > 0) {
        # files contains all files of one specific course/chapter. Notice that
        # the length of files should be lgts[j]. The procedure is the following:
        #   1. Split the file name at first "_". This splits e.g.
        #      ".../slides_about_r.Rmd" into ".../slides" and "about_r.Rmd".
        #   2. Just keep the second string and delete the file extension. This is
        #      done with "start = 1" and "stop = nchar(str) - 4".
        files = unlist(lapply(X = strsplit(x = slides[[i]][[j]], split = "_"),
          FUN = function (x) {
            str = paste(x[-1], collapse = "_")
            substr(x     = str,
              start = 1,
              stop  = nchar(str) - 4)
          }))

        temp = data.frame(
          course   = courses[i],
          chapters = chapter[[i]][j],
          files    = files
        )

        struct = rbind(struct, temp)
      }
    }
  }
  return(struct)
}
