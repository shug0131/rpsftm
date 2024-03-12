
#' A function taken from the survival library - it is not exported from there hence a local copy
#' 
#'
#'@title termsinner
#'
#'@return a list of variables referred to in a formula or call object
#'@param x an R object
#'@param ... extendible arguments to the S3 method
#'@return a character vector
#'@importFrom survival Surv
#'@keywords internal

termsinner <- function (x,...) 
{
  #if (class(x) == "formula") 
  #  c(terms.inner(x[[2]]), terms.inner(x[[3]]))
  #else 
  if (inherits(x,"call") && (x[[1]] %neq% as.name("$") && 
                                  x[[1]] %neq% as.name("["))) {
    if (x[[1]] %eq% as.name("+") || x[[1]] %eq% as.name("*") || x[[1]] %eq% as.name("-")) {
      c(termsinner(x[[2]]), termsinner(x[[3]]))
    }
    else if (x[[1]] %eq% as.name("Surv") || x[[1]] %eq% as.name("rand")) 
      unlist(lapply(x[-1], termsinner))
    else termsinner(x[[2]])
  }
  else (deparse(x))
}

"%eq%" <- function(x,y){
  identical(x,y)
}

"%neq%" <- function(x,y){
  !identical(x,y)
}
