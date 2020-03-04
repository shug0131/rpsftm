
conf_interval <- function(alpha, object_inputs,psi_hat, ci_search_limit){
  #find roots of profile(x, index,....)=target
  
 
  target <- c(qnorm(alpha/2), qnorm(1-alpha/2))
  if(1<length(psi_hat)){target <- target^2}
  n_events=sum(object_inputs$response[,2])
  dim_psi <- length(psi_hat)
  CI <- data.frame(lower=rep(NA,dim_psi), upper=rep(NA, dim_psi))
  
  if(is.null(ci_search_limit)){
    limit <- rep(3*dim_psi^2/sqrt(n_events), dim_psi)#rep(10,dim_psi)#
    ci_search_limit <- cbind(-limit, limit)
  }
  
  
  for( column in 1:2){
    for(index in 1:dim_psi){
      if( dim_psi==1){ 
        interval <- psi_hat[index] + ci_search_limit[index,]
      } else{
        interval <- c(psi_hat[index], psi_hat[index]+ 100*(2*column -3)) #ci_search_limit[index,column])
      }
      limit <- try(uniroot(
        f= profile,interval=interval, 
        index=index, target=target[column],object_inputs=object_inputs,
        psi_hat=psi_hat, search_limit=ci_search_limit+psi_hat, extendInt = "no"
      )
      )
      if(class(limit)!="try-error"){CI[index,column] <- limit[1]}
    }
  }
  
  CI_order <- CI
  CI_order[,1] <- pmin(CI[,1],CI[,2])
  CI_order[,2] <- pmax(CI[,1],CI[,2])
  CI_order  
}


profile <- function(x, index, target, object_inputs, psi_hat, search_limit){
  if(length(psi_hat)==1){ 
    do.call(est_eqn_vectorize, c(list(psi=x), object_inputs)) -target
  } else{
    fixed_inputs <- list( x=x, index=index, psi_hat=psi_hat, object_inputs=object_inputs)
    if( length(psi_hat)== 2){
      action <- optimize
      #fixed_inputs <- c(fixed_inputs, list( object= min_eqn))
      # need to improve the interval limits from this hard coding.
      ans <- do.call(action, c(list(f=fix_x_simple, interval=search_limit[-index,]), 
                               fixed_inputs))
    }
    if( 2< length(psi_hat)){
      action <- optim
      #fixed_inputs <- c(fixed_inputs, list(object= min_eqn))
      ans <- do.call(action, c(list( par=psi_hat[-index],fn=fix_x_simple), fixed_inputs))
    }
    output=unlist(ans[1]) # to pick out whatever is given back first : uniroot or optim.
    # SHOULD always be est_eqn in fact...
    psi <- psi_hat
    # does this need to be vectorised to work with uniroot?? Seems not!
    psi[index] <- x
    psi[-index] <- output
    output <- do.call(min_eqn,c(list(psi=psi),object_inputs))-target
    output
  }  
}

# maybe we don't need the object argument!
# fix_x <- function(nuisance, x, index, psi_hat, object_inputs){#}, object){
#   # MAKE this work with vectorised inputs, and be able to use
#   # est_eqn_vectorize
#   p <- length(psi_hat)
#   i <- length(nuisance)
#   j <- length(x)
#   psi <- array(0, dim=c(p,i,j))
#   answer <- array(0,dim=c(i,j))
#   psi[-index,1:i,] <- nuisance
#   psi[index,,1:j] <- x
#   for( row in 1:i){
#     for(col in 1:j){
#        value <- do.call(min_eqn, c(list(psi=psi[,row,col]),object_inputs))    
#       #est_eqn gives back a vector of z-statistics! We want the element for the nuisance parameter
#       #if( p==2 & object==est_eqn){ value <- value[-index]}
#        answer[row,col] <-value
#       }
#   }
#   if( i==1 | j==1){answer <- as.vector(answer)}
#   answer
# }

fix_x_simple <- function(nuisance, x, index, psi_hat, object_inputs){#}, object){
  # MAKE this work with vectorised input, x 
  p <- length(psi_hat)
  j <- length(x)
  psi <- array(0, dim=c(p,j))
  answer <- array(0,dim=c(1,j))
  psi[-index,1:j] <- nuisance
  psi[index,1:j] <- x
      for(col in 1:j){
      value <- do.call(min_eqn, c(list(psi=psi[,col]),object_inputs))    
      #est_eqn gives back a vector of z-statistics! We want the element for the nuisance parameter
      #if( p==2 & object==est_eqn){ value <- value[-index]}
      answer[col] <-value
    }
  
  if( j==1){answer <- as.vector(answer)}
  answer
}




#fix_x_vectorise <- Vectorize(fix_x, vectorize.args=c("nuisance","x"))



#### FROM Here down is old versions, ignore (when I've debugged the code above!)


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


