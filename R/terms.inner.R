
#' A function taken from the survival library - it is not exported from there hence a local copy
#' 
#'
#'@title terms.inner
#'
#'@return a list of variables referred to in a formula or call object
#'@param x an R object
#'@return a character vector
#'@importFrom survival Surv
#'@keywords internal

terms.inner <- function (x) 
{
  #if (class(x) == "formula") 
  #  c(terms.inner(x[[2]]), terms.inner(x[[3]]))
  #else 
  if (inherits(x,"call") && (x[[1]] != as.name("$") && 
                                  x[[1]] != as.name("["))) {
    if (x[[1]] == "+" || x[[1]] == "*" || x[[1]] == "-") {
      c(terms.inner(x[[2]]), terms.inner(x[[3]]))
    }
    else if (x[[1]] == as.name("Surv") || x[[1]] == as.name("rand")) 
      unlist(lapply(x[-1], terms.inner))
    else terms.inner(x[[2]])
  }
  else (deparse(x))
}

