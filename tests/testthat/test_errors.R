library(rpsftm)
context("Test for desired errors in rpsftm")

immdef$propX <- with(immdef, 1-xoyrs/progyrs)

test_that("Errors in test argument",{
expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, propX),test=mantelhaen.test,immdef, 
              low_psi=-1, hi_psi=1), "Test must be one of: survdiff, coxph, survreg")
})




#Check up the error functions  i.e. wrong order of censoring/ events
#rx outside of [0,1]
test_that("Errors in arm >1",{
  expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, propX+1),immdef, 
                       low_psi=-1, hi_psi=1), "Invalid values for rx")
})

test_that("Errors in arm <1",{
  expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, propX-1),immdef, 
                       low_psi=-1, hi_psi=1), "Invalid values for rx")
})

test_that("Errors in randomisation out of {0,1}",{
  expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm+0.5, propX-1),immdef, 
                       low_psi=-1, hi_psi=1), "Invalid values for rx")
})


test_that("Censoring before Time warning",{
  expect_warning( rpsftm(ReCen(progyrs, censyrs*0.5)~Instr(imm, propX),immdef, 
                       low_psi=-1, hi_psi=1), "You have observed events AFTER censoring")
})

test_that("Too Many Recen() terms",
         {
          expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, 1-xoyrs/progyrs)+ReCen(censyrs,progyrs),immdef, 
                              low_psi=-1, hi_psi=1), "Recen\\(\\) term only on the LHS of the formula")
        
     }
)

test_that("No Instr() terms",
          {
            expect_error( rpsftm(ReCen(progyrs, censyrs)~imm,immdef, 
                                 low_psi=-1, hi_psi=1), "Exactly one Instr\\(\\) term allowed")
            
          }
)

test_that("Too many Instr() terms",
          {
            expect_error( rpsftm(ReCen(progyrs, censyrs)~def*Instr(imm, 1-xoyrs/progyrs),immdef, 
                                 low_psi=-1, hi_psi=1), "Exactly one Instr\\(\\) term allowed")
            
          }
)

test_that("Instr() interaction",
          {
            expect_error( rpsftm(ReCen(progyrs, censyrs)~def/Instr(imm, 1-xoyrs/progyrs),immdef, 
                                 low_psi=-1, hi_psi=1), "Instr\\(\\) term must not be in any interactions")
            
          }
)



test_that("More than 2 arms",
          {
            myarm <- factor(rep(1:4,250))
            
            expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(myarm, 1-xoyrs/progyrs),immdef, 
                                 low_psi=-1, hi_psi=1), "arm must have exactly 2 observed values")
          }
)

test_that("less than 2 arms",
          {
            
            
            expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(1, 1-xoyrs/progyrs),immdef, 
                                 low_psi=-1, hi_psi=1), "arm must have exactly 2 observed values")
          }
)



# missing data handling
#to be implimented

test_that("na actions",
          {
          index <- sample(dim(immdef)[1],20)
          myarm <- immdef$imm
          myarm[index] <- NA
          
          fit <- rpsftm(ReCen(progyrs, censyrs)~Instr(imm, 1-xoyrs/progyrs),immdef, na.action=na.fail,
                        low_psi=-1, hi_psi=1)
          expect_is(fit$psi, class="numeric")
          
          
          fit <- rpsftm(ReCen(progyrs, censyrs)~Instr(myarm, 1-xoyrs/progyrs),immdef, na.action=na.omit,
                 low_psi=-1, hi_psi=1)
          expect_is(fit$psi, class="numeric")
          expect_is(fit$na.action, class="omit")
          
          expect_error(rpsftm(ReCen(progyrs, censyrs)~Instr(myarm, 1-xoyrs/progyrs),immdef, na.action=na.fail,
                        low_psi=-1, hi_psi=1),
                       "Error in na.fail.default")
          }
          
          )

test_that("error for poor initial starting values",{
    expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm,1-xoyrs/progyrs),immdef,
               low_psi=-1, hi_psi=-0.9),
               "The starting interval"
    )
})

test_that("warning for non-convergencevalues",{
  expect_warning( fit <- rpsftm(ReCen(progyrs, censyrs)~Instr(imm,1-xoyrs/progyrs)+entry,immdef),
                "It is set to NA"
  )
  expect_equal(is.na(fit$psi),TRUE)
})