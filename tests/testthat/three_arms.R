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
rpsftm_multi(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs, method="BFGS")
fit <- rpsftm_multi(Surv(progyrs, prog)~rand(I(1-xoyrs/progyrs)~imm),immdef, censyrs, method="Nelder-Mead")

fitm <- rpsftm_multi(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
                     method="BFGS")
fitm

fitrho <- rpsftm_multi(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
             method="BFGS", rho=0.5) 

fitm <- rpsftm_multi(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime,
                     method="Nelder-Mead", test=survreg)
fitm

plot(fitm)

fit <- survdiff(Surv(survtime,status)~rx, data=df)



rpsftm_multi(Surv(survtime,status)~rand(t_p+p_p~rx)+switchtime, data=df, censor_time = censtime,
             method="Nelder-Mead", test=survreg, start=fitm$psi)
