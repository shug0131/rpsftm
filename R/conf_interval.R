#' @keywords internal
#' 
#' 
#' MAYBE this could be merged with the multi-dim case, but add in a functino for
#' error checking on top. 
conf_interval_1d <- function(){
  lower <- try(root(qnorm(1 - alpha/2)), silent = TRUE)
  upper <- try(root(qnorm(alpha/2)), silent = TRUE)
  # handle errors in root and CI finding
  lower.error <- class(lower) == "try-error"
  upper.error <- class(upper) == "try-error"
  
  if (lower.error) {
    lower <- list(root = NA)
  }
  if (upper.error) {
    upper <- list(root = NA)
  }
  if (lower.error|| upper.error) {
    warning("Evaluation of a limit of the Confidence Interval failed.  It is set to NA")
  }
  # sort the ordering
  if (!is.na(upper$root) & !is.na(lower$root) & upper$root < lower$root) {
    temp <- lower
    lower <- upper
    upper <- temp
    rm(temp)
  }
  CI=c(lower$root,upper$root)
  CI
}

#'@keywords internal

conf_interval_multi <- function(dim_psi,n_events, alpha){
  
  #dim_psi <- length(ans$root)
  #n_events <- sum(fit$n.event)
  
  CI <- data.frame(lower=rep(NA,dim_psi), upper=rep(NA, dim_psi))
  if(is.null(ci_search_limit)){
    limit <- rep(6*dim_psi/sqrt(n_events), dim_psi)
    ci_search_limit <- cbind(-limit, limit)
  }
  
  
  for( column in 1:2){
    for(index in 1:dim_psi){
      
      limit <-try(uniroot(
        find_limit,c(ans$root[index], ans$root[index]+ci_search_limit[index, column]), 
        alpha=alpha,
        n_events=n_events,
        index=index, psi_hat=ans$root,
        data = data, formula_list=formula_list, 
        treatment_matrix=treatment_matrix, 
        rand_matrix=rand_matrix,
        test = test, autoswitch = autoswitch, 
        response=Y,method=method,
        test_args = list(...)
      ))
      
      if(class(limit)!="try-error"){CI[index,column] <- limit$root}
    }
  }
  
  CI
}

#'@keywords internal

# LOOS THE ... apart from being able to add args to the fit function.

## OK but if we are 3 arms, =2 psi, so down to 1d again for CI profiling...

min_eqn_profile <- function(nuisance,x, index, test_args, ...){
  # tweak how action/object are defined to do profiling. 
  
  psi <- rep(0,length(nuisance)+1)
  psi[-index] <- nuisance
  psi[index] <- x
  # https://www.burns-stat.com/pages/Tutor/R_inferno.pdf 8.3.15 ???
  dots <- list(...)
  do.call(min_eqn, c(list(psi),dots,test_args))
}  

#'@keywords internal

find_limit <- function(x, psi_hat,alpha,n_events,
                       index, ... ) {
  outer_target=qchisq(1-alpha,df=1)
  dots <- list(...)
  # tweak action, object, arglist here
  # call root.
  
  
  if(length(psi_hat)==-2){
    optim(par=psi_hat[-index], fn=min_eqn_profile,
          target = 0, gr=NULL,
          index=index, 
          x=x,method="Brent",...)$value - outer_target
  } else{
    optim(par=psi_hat[-index], fn=min_eqn_profile,
          target = 0, gr=NULL,
          index=index, 
          x=x,...)$value - outer_target
    
    
    #limit <- 6*length(psi_hat)/sqrt(n_events)
    # I think the dots are the problem. Must match the arguments
    # to min_eqn exactly or min_eqn_profile exactly. 
    # see ?optimize
    
    #optimize(
    #  f=min_eqn_profile,
    #  interval= c(-limit,limit),
    #  target=0,index=index,x=x,
    #  ...)$minimum-outer_target
  }
}


