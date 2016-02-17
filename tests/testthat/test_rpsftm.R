library(rpsftm)
context("Test the rpsftm() function")

immdef$propX <- with(immdef, 1-xoyrs/progyrs)
fit <- rpsftm(progyrs, censyrs, propX,imm,test=survdiff,immdef, 
           lowphi=-1, hiphi=1)
phivalue <-  -0.1816406 <=fit$phi & fit$phi<= -.1806641 
ciLower <- -0.3505859<=fit$CI[1] & fit$CI[1]<= -0.3496094
ciUpper <- 0.0019531<=fit$CI[2] & fit$CI[2]<= 0.0029297

test_that("Values from a basic fit match up with the Stata output",
          {
           expect_true(phivalue)
           expect_true(ciLower)
           expect_true(ciUpper)
           })

##To be implimented

test_that("Check that a coxph model fits",
          {expect_true(TRUE)})
test_that("Check that a survreg, weibull model fits",
          {expect_true(TRUE)})
test_that("Check that a survreg, loglogistic model fits",
          {expect_true(TRUE)})


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

#CHECK that each line of code has been called somehow in this testing process??





