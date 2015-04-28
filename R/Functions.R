

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