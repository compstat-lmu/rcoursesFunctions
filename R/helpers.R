#' @title Create an exercise slide.
#'
#' @description
#'   While creating the list with the course structure, with the `nExercise`
#'   function it is very easy to include slides for the exercises. The only
#'   parameter is the number of exercise which should be shown on the slide. If
#'   you want a slide with three exercises on it, than just use `nExercise(3)`.
#'   This function is only implemented for 0 < n < 4, and creates for `n = 3`
#'   the following slide:
#'
#'     #
#'     \threeExercises
#'
#'   The main advantage of using this function is the counter used in the
#'   latex command `threeExercises`. Notice, that the counter will be
#'   resetted after every subsection!
#'
#'
#' @param n [`integer(1)`]\cr
#'   The number of exercises shown on the slide.
#' @return [`character(2)`]\cr
#'   A character vector containing the lines of the exercise slide. This vector
#'   has the class `exercise_slide` which is needed for the function
#'   `createCourse`, that it recognize how to deal with the character
#'   vector.
nExercises = function (n) {
  if (n > 3 || n < 1) {
    stop("At the moment there are just functions for 1, 2 or 3 exercises, sorry! :(")
  }

  if (n == 1) {
    fun = "oneExercise"
  }
  if (n == 2) {
    fun = "twoExercises"
  }
  if (n == 3) {
    fun = "threeExercises"
  }
  slide_ex = c("## {.plain}", paste0("\\", fun))
  class(slide_ex) = "exercise_slide"

  return(slide_ex)
}

#' @title Create an customized slide.
#'
#' @description
#'   While creating the list with the course structure, with the `slide`
#'   function it is easy to include a slide with customized content. Please
#'   use `\\\\` instead of `\\` for latex commands (see example
#'   below).
#'
#' @param ... [`character`]\cr
#'   Arguments passed to `list`. This list will then converted to a vector.
#'   Therefore be careful with type conversions. To be on the save side, just
#'   use character vectors.
#' @return [`character`]\cr
#'   A character vector containing the lines of the slide. This vector
#'   has the class `empty_slide` which is needed for the function
#'   `createCourse`, that it recognize how to deal with the character
#'   vector.
#' @examples
#'   test_slide = slide("# Test Slide",
#'     "The euclidean norm is defined by:",
#'     "\\[ \\|x\\|_2 = \\left( \\sum\\limits_{i = 1}^n x_i^2 \\right)^{1 / 2} \\]")
#'
#'   writeLines(test_slide)
slide = function (...) {
  strings = unlist(list(...))
  class(strings) = "empty_slide"

  return(strings)
}

#' @export
adjust_path = function(path_to_child_folder, rp) {
  is.child = knitr:::child_mode()
  function(rp) {
    if (is.child)
      rp = file.path(path_to_child_folder, rp)
    else
      rp = file.path("figure", rp)
    rp
  }
}
