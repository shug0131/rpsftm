library(rpsftm)
head(immdef)


myformula <- terms( Surv(progyrs,xo)~prog+imm*RC(imm, censyrs) , data=immdef, specials=c("RC"))

RC <- function(arm,obs){cbind(arm=arm, obs=obs)}

myRC <- with(immdef, RC(imm, censyrs))

RCindex <- attr(myformula,"specials")$RC
drops=attr(myformula,"factors")[RCindex,]==0
# this could be a way to induce "Helpful" errors to ensure there is only 1 RC() term and it
# only turns up once, not in any interactions.
drops <- apply(drops,2, all)

mf <- model.frame(myformula, data=immdef)
mf[,RCindex][,"arm"]
mf[,RCindex][,"obs"]

#myformulaMinus <- drop.terms( myformula, dropx= RCindex-1, keep.response=F)

formula(myformula)
formula(myformula[drops])


head(mf)
class(mf[,4])
