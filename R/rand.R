#' A function that is defined to be used in the formula argument, 
#' and identified as specials in the terms() object
#'
#'@title rand functions to use in the rpsftm() formula
#'
#'@export
#'@return matrix with two columns named arm and rx. These can be used in the formula argument to rpsftm()
#'@author Simon Bond
#'@param arm the randomised treatment arm. a factor with 2 levels, or numeric variable with values 0/1.
#'@param rx the proportion of time on active treatment (arm=1 or the non-reference level of the factor)
#'@seealso \code{\link{print.rand}}, \code{\link{rpsftm}}
#'@examples 
#'x <- with(immdef, rand(imm , 1 - xoyrs / progyrs ) )
#'x
#'class(x)
#'y <- as.data.frame(x)
#'head(y)



rand <- function(arm, rx) {
  if (is.numeric(arm) & any(!(arm %in% c(0, 1)))) {
    warning("Auto checking of no switching needs treatment to have value 0 or 1")
  }
  if (is.factor(arm)) {
    message <- paste("Auto checking of switching assumes the lowest level of arm='", 
                     levels(arm)[1], "' is the control or placebo treatment", sep = "")
    warning(message)
    # converts the numerically coding (1,2,..), to 0 or 1.
    arm <- as.numeric(arm) - 1
  }
  structure(
    cbind(arm = arm, rx = rx),
    class=c("rand", "matrix")
  )
}