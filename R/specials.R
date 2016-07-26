#' A couple of functions that are defined to be used in the formula argument, 
#' and identifed as specials in the terms() object
#'
#'@title Special Functions to use in the rpsftm() formula
#'
#'@return matrix with two columns named either time and censor_time, or, arm and rx. These can be used in the formula argument to rpsftm()
#'@author Simon Bond
#'@param arm the randomised treatment arm. a factor with 2 levels, or numeric variable with values 0/1.
#'@param rx the proportion of time on active treatment (arm=1 or the non-reference level of the factor)
#'@describeIn rand rand function

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
  cbind(arm = arm, rx = rx)
}

#'@describeIn rand ReCen function
#'@param time the observed failure or censoring time
#'@param censor_time the time at which censoring would, or has occurred. This is provided for all observations
#' unlike standard Kaplan-Meier or Cox regression where it is only given for censored observations

ReCen <- function(time, censor_time) {
  cbind(time = time, censor_time = censor_time)
}
