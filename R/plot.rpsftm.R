#'Function used to plot the KM curves of the treatment-free transformed times
#'
#'@export
#'@title Plot Method
#'@name plot.rpsftm
#' @param x an object returned from the \code{\link{rpsftm}} function.
#' @param ... further arguments passed to or from other methods.
#' @return a ggplot plot of the fitted KM curves. The underlying data.frame has variables
#' \itemize{
#' \item time: failure time
#' \item survival: estimated treatment-free survival probability 
#' \item upper: upper confidence interval at level defined by alpha in the call to rpsftm
#' \item lower: lower confidence interval at level defined by alpha in the call to rpsftm
#' \item group: randomised treatment arm
#' }
#' @author Simon Bond
#' @examples 
#' fit <- rpsftm(Surv(progyrs, prog)~rand(imm,1-xoyrs/progyrs),immdef, censyrs)
#' plot(fit)
#' library(ggplot2)
#' plot(fit)+
#'    scale_linetype_discrete(labels=c("Control","Experimental"))+
#'    ylim(0.5,1)+
#'    geom_ribbon(aes(ymin=lower, ymax=upper, fill=group), alpha=0.3)+
#'    labs(x="Time (years)", title=NULL, lty="Arm", fill="Arm")


plot.rpsftm <- function(x,...) {
  fit <- x$fit
  df <- data.frame(time = fit$time, survival = fit$surv, upper = fit$upper, 
                   lower = fit$lower)
  df$group <- rep(names(fit$strata), fit$strata)
  ggplot2::ggplot(data = df, ggplot2::aes_string(x = "time", y = "survival", group = "group", 
                                          lty = "group")) + 
    ggplot2::geom_step() + ggplot2::ylim(0, 1) + 
    ggplot2::labs(title = "KM Plots of Transformed Treatment-Free Time")
  
  
}