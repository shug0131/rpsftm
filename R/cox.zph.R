#create method cox.zph

cox.zph <- function(fit,...){
  UseMethod("cox.zph")
}

# call directly survival.cox.zph for anything other than rpsftm objects.

cox.zph.default<- function(fit,...){
  survival::cox.zph(fit,...=...)
}

# for rpsftm objects use
cox.zph.rpsftm <- function(fit,...){
  if( !inherits(fit, "coxph")){
    stop("This model did not use coxph within g-estimation")
  } else {
    survival::cox.zph(attr(fit$ans$f.root,"fit"),...=...)    
  }
}
