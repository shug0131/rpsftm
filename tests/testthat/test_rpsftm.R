library(rpsftm)
context("Test the rpsftm() function")

propX <- with(immdef, 1-xoyrs/progyrs)

openlines <- function(time, censor_time, rx, arm,data, 
                              formula=~1, test=survdiff, 
                              lowphi=-10,hiphi=10, alpha=0.05,
                              Recensor=TRUE,Autoswitch=TRUE, ...){
  cl <- match.call()
  
  #create formula for fitting, and to feed into model.frame()
  if(is.null(formula)){
    formula <- as.formula("~1")
    cl$formula <-formula 
  }
  #from the lm() function
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data","time", "censor_time", "rx", "arm"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::get_all_vars)
  #mf$formula <- terms(formula)
  #this returns a data frame with all the variables in and renamed time, censor_time, rx, arm as appropriate
  df <- eval(mf, parent.frame())
  return(df)
}

lmopen <- function (formula=~1, data, arm, subset, weights, na.action, method = "qr", 
                    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
                    contrasts = NULL, offset, ...) 
{
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "subset", "weights", "na.action", "arm",
               "offset"), names(mf), 0L)
  
  mf <- mf[c(1L, m)]
  print(class(mf[2]))
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  formula(mf) <- formula
  mf <- eval(mf, parent.frame())
  return(mf)
}


test_that("lm check",{
  propX <- with(immdef,1-xoyrs/progyrs)
  print(head(lmopen(data=immdef, arm=propX, formula=~1)))
  
})


test_that("first basict fit with mixed data sources",{
  propX <- with(immdef,1-xoyrs/progyrs)
  fit <- openlines(progyrs, censyrs,propX,imm,immdef,#formula=~1,
           lowphi=-1, hiphi=1)
  print(head(fit))
})
#
test_that("with no data argument at all",{
  propX <- with(immdef, 1-xoyrs/progyrs)
  fit <- rpsftm(immdef$progyrs, immdef$censyrs, propX,immdef$imm, 
              lowphi=-1, hiphi=1)
  
}
)






test_that("Values from a basic fit match up with the Stata output",
          {
            propX <- with(immdef, 1-xoyrs/progyrs)
            fit <- rpsftm(immdef$progyrs, immdef$censyrs, propX,immdef$imm, 
                          lowphi=-1, hiphi=1)
            phivalue <-  -0.1816406 <=fit$phi & fit$phi<= -.1806641 
            ciLower <- -0.3505859<=fit$CI[1] & fit$CI[1]<= -0.3496094
            ciUpper <- 0.0019531<=fit$CI[2] & fit$CI[2]<= 0.0029297
           expect_true(phivalue)
           expect_true(ciLower)
           expect_true(ciUpper)
           })





##Check that swapping the definitions of arm has no effect (on point estimates), and that 1-rx reverses the estimates and CIs

test_that("swapping the definition of arm",
          {
            propX <- with(immdef, 1-xoyrs/progyrs)
            fitInv <- rpsftm(progyrs, censyrs, propX,def,test=survdiff,immdef, 
                             lowphi=-1, hiphi=1)
            expect_true(  abs(fit$phi-fitInv$phi)<1e-4)}
          )
test_that("swapping the definition of rx",
          {propXInv <- 1-with(immdef, 1-xoyrs/progyrs)
          fitInv2 <- rpsftm(progyrs, censyrs, propXInv,imm,test=survdiff,immdef, 
                            lowphi=-1, hiphi=1)
            expect_true(  abs(fit$phi+fitInv2$phi)<1e-4)}
)

test_that("swapping the definition of arm and rx",
          {
            propXInv <- 1-with(immdef, 1-xoyrs/progyrs)
            fitInv3 <- rpsftm(progyrs, censyrs, propXInv,def,test=survdiff,immdef, 
                              lowphi=-1, hiphi=1)
            expect_true(  abs(fit$phi+fitInv3$phi)<1e-4)
            expect_true(  abs(fit$CI[1]+fitInv3$CI[2])<1e-4)
            expect_true(  abs(fit$CI[2]+fitInv3$CI[1])<1e-4)
            }
)

test_that( "no t-test comparison avaialable",
           {propX <- with(immdef,1-xoyrs/progyrs)
           fit <- rpsftm(progyrs, censyrs, propX,imm,immdef, 
                         lowphi=-1, hiphi=1)
           expect_error( update(fit, test=t.test))}
)



test_that( "check variants on fitting",
           {propX <- with(immdef,1-xoyrs/progyrs)
           fit <- rpsftm(progyrs, censyrs, propX,imm,immdef, 
                         lowphi=-1, hiphi=1)
           
           f0 <- fit
           f1 <- update(fit, test=coxph)
           f2 <- update(fit, test=survreg, dist="weibull")
           f3 <- update(fit, test=survreg, dist="exponential")
           f4 <- update(fit, test=survreg, dist="loglogistic")
           f5 <- update(fit, test=survreg, dist="gaussian")
           f6 <- update(fit, test=survreg, dist="lognormal")
           
           
           
           
           fits <- list(f0,f1,f2,f3,f4,f5,f6)
             
             
             lapply(fits, expect_is, class="rpsftm")}
           )
## need to create some data for strata cluster, covariates.




##These may need to be specific to different fit, ie. adjustment is different for survdiff and coxph.
test_that("Check that a strata fits",
          {
            propX <- with(immdef,1-xoyrs/progyrs)
            f0 <- rpsftm(progyrs, censyrs, propX,imm,immdef, 
                          lowphi=-1, hiphi=1)
            category <- rep(c("A","B","C","D"),rep(250,4))
            covar <- rnorm(1000)
            clusterId <- rep(1:100,10)
            f0.strata <- update(f0,~.+strata(category))
            f1 <- update(f0, test=coxph)
            f1.All <- update(f1,~.+strata(category)+covar+cluster(clusterId))
            
            expect_is(f0.strata, class="rpsftm")
            expect_is(f1.All, class="rpsftm")
            })

test_that("Check that a cluster fits",
          {expect_true(TRUE)})

test_that("Check that a covariate adjustment fits",
          {expect_true(TRUE)})
test_that("Check that a covariate interaction adjustment fits",
          {expect_true(TRUE)})

#Drill down into the component functions, maybe with individual files:
# EstEqn
# ExtractZ
# plot/print/summary
# recensor

#Check up the error functions  i.e. wrong order of censoring/ events
#rx outside of [0,1]
# missing data handling



# call data from inside the data argument and outside, to check this is ok.??


#CHECK that each line of code has been called somehow in this testing process??





