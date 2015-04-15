Recensor=function(phi,Time,CensorTime,Rx){
  require(survival)
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
Test=function(phi,Time,CensorTime,Rx,Arm, target=0){
  require(survival)
  Sstar=Recensor( phi,Time,CensorTime,Rx)
  # needs a check that Arm is two levels
  if( length(unique(Arm))!=2){stop("Arm must have exactly 2 observed values")}
  # Might want to generalise to allow other tests, and adjustors.
  fit=survdiff( Sstar~Arm)
  z=(fit$exp-fit$obs)/sqrt(diag(fit$var))
  fit$chisq
  z[1]-target
}



rpsftm=function(Time, CensorTime, Rx, Arm,data, lowphi=-10,hiphi=10, alpha=0.05){
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
              target=0)
  lower=uniroot(Test, c(lowphi,hiphi), 
                Time=Time, CensorTime=CensorTime, Rx=Rx, Arm=Arm, 
                target=qnorm(1-alpha/2))
  upper=uniroot(Test, c(lowphi,hiphi), 
                Time=Time, CensorTime=CensorTime, Rx=Rx, Arm=Arm, 
                target=qnorm(alpha/2))
  

  
  phiHat=ans$root
  Sstar=Recensor(phiHat, Time, CensorTime,Rx)
  #not sure about this, maybe survfit(Sstart~1)??
  fit=survfit(Sstar~Arm)
  
  
  list(phi=phiHat, 
       fit=fit, 
       Sstar=Sstar, 
       ans=ans, 
       CI=c(lower$root,upper$root),
       call=match.call())
  
}