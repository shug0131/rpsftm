Recensor=function(phi,Time,CensorTime,Rx){
  require(survival)
  #Put in the data= argument and process.
    
  if( any (!(0<=Rx & Rx<=1))){stop("Invalid values for Rx. Must be proportions in [0,1]")}
  if( any(CensorTime<Time)){warning("You have observed events AFTER censoring. These are handled as censored")}
  U=Time*((1-Rx)+Rx*exp(phi))
  Cstar=pmin(CensorTime, CensorTime*exp(phi))
  Tstar=pmin(U,Cstar)
  deltaStar=1*(U<Cstar)
  Surv(Tstar,deltaStar)
}

# do a log-rank test
# Could generalise this to other tests.
#build a formula style interface
Test=function(phi,Time,CensorTime,Rx,Arm, data, adjustors, target=0, test, ...){
  require(survival)
  Sstar=Recensor( phi,Time,CensorTime,Rx)
  data=cbind(Sstar, data)
  # needs a check that Arm is two levels
  if( length(unique(Arm))!=2){stop("Arm must have exactly 2 observed values")}
  # Might want to generalise to allow other tests, and adjustors.
  #build a formula object,
  if(is.null(adjustors)){
    MyFormula=as.formula("~1")
  }else{
    MyFormula= as.formula(adjustors)
  }
  MyFormula=update(MyFormula, Sstar~.+Arm)
  
  # mycall=list(test, formula, data,...)
  # mycall=as.call(mycall)
  # fit=eval(mycall)
  
  
  fit=survdiff( MyFormula,data)
  
  ExtractZ(fit)-target
  
}

ExtractZ=function(x,...){UseMethod("ExtractZ")}
ExtractZ.survdiff=function(fit){
  if (is.matrix(fit$obs)) {
    otmp <- apply(fit$obs, 1, sum)
    etmp <- apply(fit$exp, 1, sum)
  }
  else {
    otmp <- fit$obs
    etmp <- fit$exp
  }
  df <- (etmp > 0)
  if (sum(df) < 2) 
    chi <- 0
  else {
    temp2 <- ((otmp - etmp)[df])[-1]
    vv <- (fit$var[df, df])[-1, -1, drop = FALSE]
    #chi <- sum(solve(vv, temp2) * temp2)
    z<- temp2/sqrt(vv)
  }
  z
}






rpsftm=function(Time, CensorTime, Rx, Arm,data, adjustors=NULL, test=logrank, lowphi=-10,hiphi=10, alpha=0.05,...){
  #input
  # Time survival time/or censoring time
  # Rx proportion of time on treatment
  # CensorTime censoring time - including the theoretical one for those observed
  # Arm factor giving the treatment arm
  
  #define funtion of phi
  require(survival)
  
  if(!missing(data)){
    Time <- data[,deparse(substitute(Time))]
    CensorTime <- data[,deparse(substitute(CensorTime))]
    Rx <-data[,deparse(substitute(Rx))]
    Arm <-  data[,deparse(substitute(Arm))]
  }
  
  #check or handle missing data.
  
 
  
  #argument for adjustors or formula//
  #argument for a different test other than log rank.
  
  #solve to find the value of phi that gives the root to z=0
  ans=uniroot(Test, c(lowphi,hiphi), 
              Time=Time, CensorTime=CensorTime, Rx=Rx, Arm=Arm, 
              data=data, adjustors=adjustors,
              target=0)
  lower=uniroot(Test, c(lowphi,hiphi), 
                Time=Time, CensorTime=CensorTime, Rx=Rx, Arm=Arm, 
                data=data, adjustors=adjustors,
                target=qnorm(1-alpha/2))
  upper=uniroot(Test, c(lowphi,hiphi), 
                Time=Time, CensorTime=CensorTime, Rx=Rx, Arm=Arm, 
                data=data, adjustors=adjustors,
                target=qnorm(alpha/2))
  

  
  phiHat=ans$root
  Sstar=Recensor(phiHat, Time, CensorTime,Rx)
  #not sure about this, maybe survfit(Sstart~1)??
  
  #ought to make this a function to use in test as well
  
  if(is.null(adjustors)){
    MyFormula=as.formula("~1")
  }else{
    MyFormula= as.formula(adjustors)
  }
  MyFormula=update(MyFormula, Sstar~.+Arm)
  
  
  fit=survfit(MyFormula, data)
  
  
  list(phi=phiHat, 
       fit=fit, 
       Sstar=Sstar, 
       ans=ans, 
       CI=c(lower$root,upper$root),
       call=match.call())
  
}