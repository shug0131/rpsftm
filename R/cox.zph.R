#' Test the proportional hazards assumption of an RPSFTM/Cox Regression
#' @param fit the result of fitting a rpsftm model using \code{\link[survival]{coxph}} as the inner estimation tool.
#' @param ... any other arguments to pass to \code{\link[survival]{cox.zph}}.
#' 
#' @description If the the \code{fit} inherits *both* \code{rpsftm} and \code{coxph} then this pulls out the genuine 
#' \code{survival::coxph} object that is deeply nested in the object, and then runs \code{survival::cox.zph} on it.
#' Or it avoids overwriting the function from \code{survival} by calling \code{survival::cox.zph} directly if the object 
#' does not inherit \code{rpsftm}. Or it fails.
#' 
#' @note This does rely on the order of loading packages. The \code{rpsftm} package must be loaded after \code{survival}, 
#' if both are required, to avoid the masking of synonymous functions causing errors. 
#' 
#' @seealso  \code{\link[survival]{cox.zph}}
#' @export


cox.zph <- function(fit,...){
  if(inherits(fit, "rpsftm")){
    if( !inherits(fit, "coxph")){
      stop("This model did not use coxph within g-estimation")
    } else {
      fit <- attr(fit$ans$f.root,"fit")
    }
  }
  survival::cox.zph(fit,...=...)
}
