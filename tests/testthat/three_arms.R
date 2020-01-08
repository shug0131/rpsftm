df <- read.csv(file=system.file("extdata","sim_data.csv",package="rpsftm"), stringsAsFactors = FALSE)
df <- read.csv(file="V:/STATISTICS/NON STUDY FOLDER/Staff/Annabel/R code/rpsftm/sim_data.csv", stringsAsFactors = FALSE)

subset(df, complier==1 & switchtime<survtime)

libs <- c("magrittr", "dplyr")
for( lib in libs){ library(lib, character.only = TRUE)}

df %<>% mutate( censtime=6, 
                p1 = ifelse(complier,1, pmin(1, switchtime/survtime)),
                p2 = ifelse(complier,0,1-p1),
                r_p = case_when(rx=="R"~p1,
                                newtrt=="R"~p2,
                                TRUE~0
                                ),
                t_p = case_when(rx=="T"~p1,
                                newtrt=="T"~p2,
                                TRUE~0
                ),
                p_p = case_when(rx=="P"~p1,
                                newtrt=="P"~p2,
                                TRUE~0
                ),
                rx=factor(rx, levels=c("R","P","T"))
                )

rpsftm(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs)
#rpsftm(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs, method="BFGS")
fit <- rpsftm(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs,test=coxph)

library(profvis)
profvis({

  fitm <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime, start=c(0,0),
                 autoswitch=TRUE
                 )
})
fitm

fitrho <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
             method="BFGS", rho=0.5) 

fitreg <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
                     method="BFGS", test=survreg)
#fails with invalid survival times for distribution...
fitcox <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx)+switchtime, data=df, censor_time = censtime,
                 test=coxph)
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
