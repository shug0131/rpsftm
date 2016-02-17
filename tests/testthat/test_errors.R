library(rpsftm)
context("Test for desired errors in rpsftm")

immdef$propX <- with(immdef, 1-xoyrs/progyrs)

test_that("Errors in test argument",{
expect_error( rpsftm(progyrs, censyrs, propX,imm,test=mantelhaen.test,immdef, 
              lowphi=-1, hiphi=1))
})

#to be implimented

test_that("using arm to adjust gives an error somehow??",
          {expect_true(TRUE)}
          )

test_that("Cases with missing data, or computations in arguments?",
          {expect_true(TRUE)}
)

test_that("Non convergence ?? warnings??",
          {expect_true(TRUE)}
)

# Should issue a warning if the opening bounds for phi fail to give opposite signs to EstEqn.