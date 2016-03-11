library(rpsftm)
context("Test the rpsftm() function")

immdef$propX <- with(immdef, 1-xoyrs/progyrs)
fit <- rpsftm(progyrs, censyrs, propX,imm,immdef, 
           lowphi=-1, hiphi=1)
phivalue <-  -0.1816406 <=fit$phi & fit$phi<= -.1806641 
ciLower <- -0.3505859<=fit$CI[1] & fit$CI[1]<= -0.3496094
ciUpper <- 0.0019531<=fit$CI[2] & fit$CI[2]<= 0.0029297



fitInv <- rpsftm(progyrs, censyrs, propX,def,test=survdiff,immdef, 
       lowphi=-1, hiphi=1)
immdef$propXInv <- 1-immdef$propX 
fitInv2 <- rpsftm(progyrs, censyrs, propXInv,imm,test=survdiff,immdef, 
                  lowphi=-1, hiphi=1)
fitInv3 <- rpsftm(progyrs, censyrs, propXInv,def,test=survdiff,immdef, 
                  lowphi=-1, hiphi=1)

test_that("Values from a basic fit match up with the Stata output",
          {
           expect_true(phivalue)
           expect_true(ciLower)
           expect_true(ciUpper)
           })

##Check that swapping the definitions of arm has no effect (on point estimates), and that 1-rx reverses the estimates and CIs

test_that("swapping the definition of arm",
          {expect_true(  abs(fit$phi-fitInv$phi)<1e-4)}
          )
test_that("swapping the definition of rx",
          {expect_true(  abs(fit$phi+fitInv2$phi)<1e-4)}
)

test_that("swapping the definition of arm and rx",
          {expect_true(  abs(fit$phi+fitInv3$phi)<1e-4)
            expect_true(  abs(fit$CI[1]+fitInv3$CI[2])<1e-4)
            expect_true(  abs(fit$CI[2]+fitInv3$CI[1])<1e-4)
            }
)


f0 <- fit
f1 <- update(fit, test=coxph)
f2 <- update(fit, test=survreg, dist="weibull")
f3 <- update(fit, test=survreg, dist="exponential")
f4 <- update(fit, test=survreg, dist="loglogistic")
f5 <- update(fit, test=survreg, dist="gaussian")
f6 <- update(fit, test=survreg, dist="lognormal")


test_that( "no t-test comparison avaialable",
           {expect_error( update(fit, test=t.test))}
)

fits <- list(f0,f1,f2,f3,f4,f5,f6)

test_that( "check variants on fitting",
           {lapply(fits, expect_is, class="rpsftm")}
           )
## need to create some data for strata cluster, covariates.

#update(f0,~.+strata(def))

##These may need to be specific to different fit, ie. adjustment is different for survdiff and coxph.
test_that("Check that a strata fits",
          {expect_true(TRUE)})

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





