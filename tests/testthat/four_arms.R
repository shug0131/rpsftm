df <- read.csv(file=system.file("extdata","sim_data.csv",package="rpsftm"), stringsAsFactors = FALSE)
df <- read.csv(file="V:/STATISTICS/NON STUDY FOLDER/Staff/Annabel/R code/rpsftm/sim_data2.csv", stringsAsFactors = FALSE)

df <- read.csv(file="U:/My Documents/R/rpsftm/rpsftm/tests/simulation/sim_data2.csv")

subset(df, complier==1 & switchtime<survtime)

libs <- c("magrittr", "dplyr")
for( lib in libs){ library(lib, character.only = TRUE)}

df %<>% mutate(
                p1 = ifelse(complier,1, pmin(1, switchtime/survtime)),
                p2 = ifelse(complier,0,1-p1),
                p_ref= (rx=="Placebo")*p1+(newtrt=="Placebo")*p2,
                p_A= (rx=="Treat 1")*p1+(newtrt=="Treat 1")*p2,
                p_B= (rx=="Treat 2")*p1+(newtrt=="Treat 2")*p2,
                p_C= (rx=="Treat 3")*p1+(newtrt=="Treat 3")*p2,
                
              )
fitm4 <- rpsftm(Surv(survtime, status)~rand(p_A+p_B+p_C~rx),data=df,
                censor_time=censtime
)
summary(fitm4)



rpsftm(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs)
#rpsftm(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs, method="BFGS")
fit <- rpsftm(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs,test=coxph)

library(profvis)
profvis({
  fitm4 <- rpsftm(Surv(survtime, status)~rand(p_A+p_B+p_C~rx),data=df,
                  censor_time=censtime
                  )
  
  fitm <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime, start=c(0,0),
                 autoswitch=TRUE
                 )
  fitm1 <- rpsftm(Surv(survtime,status)~rand(p_p+t_p~rx), data=df, censor_time = censtime, start=c(0,0),
                 autoswitch=TRUE
  )
  
  
  fitcox1 <- rpsftm(Surv(survtime,status)~rand(p_p+t_p~rx), data=df, censor_time = censtime, start=c(0,0),
                test=coxph
  )
})
summary(fitm)
summary(fitm1)

summary(fitcox1)

fitrho <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
             method="BFGS", rho=0.5) 

fitreg1 <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
                      test=survreg)
fitreg2 <- rpsftm(Surv(survtime,status)~rand(p_p+t_p~rx), data=df, censor_time = censtime,
                  test=survreg)

summary(fitreg1)
summary(fitreg2)

#fails with invalid survival times for distribution...
fitcox <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
                 test=coxph,start=c(0,0))#, method="Nelder-Mead", start=fitm$psi+0.05)
summary(fitcox)
summary(fitcox1)
# gets to a place with no non-missing obs when searching over psi...

fitreg
fitreg$CI
plot(fitreg)

fit <- survdiff(Surv(survtime,status)~rx, data=df)



rpsftm_multi(Surv(survtime,status)~rand(t_p+p_p~rx)+switchtime, data=df, censor_time = censtime,
             method="Nelder-Mead", test=survreg, start=fitm$psi)

X <- matrix(runif(12), ncol=3)
pmin(list(X[,1],X[,2],X[,3]))


trt_mat <- model.matrix(~t_p+p_p, data=df)
rx_mat <- model.matrix(~rx, data=df)
Y <- with(df, Surv(survtime, status))
cens <- df$censtime
dim(trt_mat)
length(cens)
psi <- matrix(0, nrow=3000, ncol=2)
U <- untreated(psi, Y, trt_mat,rx_mat, cens, autoswitch=FALSE)
length(U)
