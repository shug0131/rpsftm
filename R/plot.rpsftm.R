#'Function used to plot the KM curves of the treatment-free transformed times
#'
#'@export
#'@title Plot Method
#'@name plot.rpsftm
#' @param x an object returned from the \code{rpsftm()} function
#' @return a ggplot plot of the fitted KM curves 
#' @author Simon Bond
#' 


plot.rpsftm=function(x){
  fit=x$fit
  
  df=data.frame(Time=fit$time, Survival=fit$surv, upper=fit$upper, lower=fit$lower )
  df$Group=rep(names(fit$strata),fit$strata)
  
  ggplot2::ggplot( data=df,ggplot2::aes(x=Time, y=Survival, group=Group, lty=Group) )+
    ggplot2::geom_step()+
    ggplot2::ylim(0,1)+
    ggplot2::labs(title="KM Plots of Transformed Treatment-Free Time")
  
  
}