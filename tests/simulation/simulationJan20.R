library(evd)
library(dplyr)
library(rpsftm)
library(magrittr)


simulate <- function(){
  # Create Data
  beta0func <- function(x, prob, cens_time){
    qexp(prob, x) - cens_time
  }
  
  
  n         <- 3000 # total sample size (approx. n/3 per group)
  cens_time <- 6 # censoring cut-off time
  deathrate <- 0.8 # death rate in active reference arm
  beta0     <- log(uniroot(beta0func, c(0, 1), prob = deathrate, cens_time = cens_time)$root)
  beta1     <- 1 # 1 = outcome dependent non-comp, 0 = independent
  
  
  gamma0    <- log(0.136) # corresponds to 30% non-compliance
  gamma1    <- 1 # 1 = outcome dependent non-comp, 0 = independent
  phip      <- log(2.7183) # based on active ref effect of h_P/h_R = 2.7183
  phit      <- 0.5 * phip # true retention effect equal to non-inferiority margin of 0.5
  
  #------------------------------------------------------------------#
  # Step 1 
  # - draw g1 ~ U(0, 1) = randomised trt assignment
  # - draw g2 ~ B(1, 0.5) = trt group which non-complier switched to 
  #------------------------------------------------------------------#
  
  g1 <- runif(n, 0, 1)
  g2 <- rbinom(n, 1, 0.5)
  
  #------------------------------------------------------------------#
  # Step 2
  # - if g1 <= 1/3, rx = P
  # - if 1/3 < g1 <= 2/3, rx = R
  # - if 2/3 < g1 <= 1, rx = T
  # where rx = randomised group, P = placebo, R = active ref, and
  # T = test group
  #------------------------------------------------------------------#
  
  rx <- ifelse(g1 <= 1/3, 
               "P",
               ifelse(1/3 < g1 & g1 <= 2/3, 
                      "R",
                      "T"))
  
  #------------------------------------------------------------------#
  # Step 3
  # - draw L1 ~ N(0, 1)
  # - define L = (1, L1)^T
  #------------------------------------------------------------------#
  
  L1 <- rnorm(n, 0, 1)
  L  <- t(matrix(c(rep(1, n), L1), ncol = 2))
  
  #------------------------------------------------------------------#
  # Step 4
  # - draw U using log(U) = -L^T*beta + eps
  # - beta = (beta0, beta1)^T
  # - eps is from extreme value distn: P(eps <= x) = 1 - exp(-exp(x))
  #------------------------------------------------------------------#
  
  beta  <- matrix(c(beta0, beta1), ncol = 1)
  eps   <- rgumbel(n, 0, 1)
  U     <- exp(-t(L) %*% beta + eps)
  
  #------------------------------------------------------------------#
  # Step 5
  # - D = potential time for subject from randomisation to non-comp
  # - draw D using model in Step 4 with beta = (gamma0, gamma1)
  # where gamma0 corresponds to proportion of non-comp
  # beta1/gamma1 = 1 means D and U are dependent
  # beta1/gamma1 = 0 means D and U and independent
  #------------------------------------------------------------------#
  
  gamma  <- matrix(c(gamma0, gamma1), ncol = 1)
  eps    <- rgumbel(n, 0, 1)
  D      <- exp(-t(L) %*% gamma + eps)
  
  #------------------------------------------------------------------#
  # Step 6
  # - calculate potential survival times V
  # - V = U * exp(-phip) if rx = P
  # - V = U * exp(-phit) if rx = T
  # - V = U if rx = R
  # if V < D then complier, if V > D then px switched
  # - calculate Dstar = min(D, V) and create compliance indicator
  #------------------------------------------------------------------#
  
  V <- ifelse(rx == "P",
              U * exp(-phip),
              ifelse(rx == "T",
                     U * exp(-phit),
                     U))
  Dstar    <- pmin(D, V)
  complier <- 1 * (V < D)
  
  #------------------------------------------------------------------#
  # Step 7
  # - calculate time from randomisation until failure event
  # - X = Dstar + (V - Dstar) * exp(phip) if rx = P and g2 = 0
  # - X = Dstar + (V - Dstar) * exp(phip) * exp(-phit) if rx = P and g2 = 1
  # - X = Dstar + (V - Dstar) * exp(-phip) if rx = R and g2 = 1
  # - X = Dstar + (V - Dstar) * exp(-phit) if rx = R and g2 = 1
  # - X = Dstar + (V - Dstar) * exp(phit) * exp(-phip) if rx = T and g2 = 0
  # - X = Dstar + (V - Dstar) * exp(phit) if rx = T and g2 = 1
  #------------------------------------------------------------------#
  
  X <- case_when(rx == "P" & g2 == 0 ~ Dstar + (V - Dstar) * exp(phip),
                 rx == "P" & g2 == 1 ~ Dstar + (V - Dstar) * exp(phip) * exp(-phit),
                 rx == "R" & g2 == 0 ~ Dstar + (V - Dstar) * exp(-phip),
                 rx == "R" & g2 == 1 ~ Dstar + (V - Dstar) * exp(-phit),
                 rx == "T" & g2 == 0 ~ Dstar + (V - Dstar) * exp(phit) * exp(-phip),
                 rx == "T" & g2 == 1 ~ Dstar + (V - Dstar) * exp(phit))
  
  # create new indicator for switched treatment
  newtrt <- case_when(complier == 1 ~ rx,
                      rx == "P" & g2 == 0 ~ "R",
                      rx == "P" & g2 == 1 ~ "T",
                      rx == "R" & g2 == 0 ~ "P",
                      rx == "R" & g2 == 1 ~ "T",
                      rx == "T" & g2 == 0 ~ "P",
                      rx == "T" & g2 == 1 ~ "R")
  
  #------------------------------------------------------------------#
  # Step 8
  # - calculate observed failure time Xstar = min(X, cens_time) 
  # time and observed censoring indicator delta = I(Xstar = X)
  #------------------------------------------------------------------#
  
  Xstar <- pmin(X, cens_time)
  delta <- 1 * (Xstar == X)
  
  #------------------------------------------------------------------#
  # Step 9
  # - create data frame of required variables
  #------------------------------------------------------------------#
  
  # rx         = randomised treatment
  # survtime   = survival or censoring time
  # status     = survival status (1 = dead, 0 = censored)
  # complier   = compliance indicator (1 = compliant, 0 = non-compliant)
  # switchtime = time patient switched treatment (= survtime for compliers)
  # newtrt     = treatment switched to
  df <- data.frame(rx         = rx,
                   survtime   = Xstar,
                   failtime   = X,
                   status     = delta,
                   complier   = complier,
                   switchtime = Dstar,
                   newtrt     = newtrt) 
  #Step 10 Convert to the desired format for rpsftm()
  
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
  
  
  # Analyse
  
  fit <- rpsftm(Surv(survtime,status)~rand(t_p+p_p~rx), data=df, censor_time = censtime)
  # output results
  list(psi=fit$psi, ci=fit$CI)
}


results <- list()
nsim <- 10000
system.time(
for( iter in 1:nsim){
  ans <- try(simulate())
  results <- c(results, list(ans))
}
)

save(results,file="tests/simulation/results.Rdata" )

psi_true <- c(0.5,1)

compute <- sapply(results, function(x){(class(x)!="try-error")})
mean_compute <- mean(compute)
results <- results[compute]

delta <- sapply(results, function(x){x$psi})-psi_true
bias <- apply(delta,1, mean)
rmse <- sqrt(apply(delta^2, 1, mean))
cis <- lapply(results, function(x){x$ci})
width <- sapply(cis, function(x){x[,"upper"]-x[,"lower"]})
mean_ci_width <- apply(width,1, mean)
cover <- sapply(cis, function(x, psi_true){x$lower<psi_true & psi_true < x$upper}, psi_true=psi_true)
coverage <- apply(cover,1, mean)




index <- sapply(cis, function(x){!any(is.na(x[2,]))})
cis2 <- cis[index]
width <- sapply(cis2, function(x){x[,"upper"]-x[,"lower"]})
mean_ci_width <- apply(width,1, mean)
cover <- sapply(cis2, function(x, psi_true){x$lower<psi_true & psi_true < x$upper}, psi_true=psi_true)
coverage <- apply(cover,1, mean)

OC <- data.frame(
  psi=psi_true,
  bias=bias,
  rmse=rmse,
  mean_ci_width=mean_ci_width,
  coverage=coverage
)
OC
save(OC, file="tests/simulation/OC.Rdata")
