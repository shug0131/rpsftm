library(rpsftm)
context("Test for desired errors in rpsftm")

immdef$propX <- with(immdef, 1-xoyrs/progyrs)

test_that("Errors in test argument",{
expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, propX),test=mantelhaen.test,immdef, 
              lowphi=-1, hiphi=1), "Test must be one of: survdiff, coxph, survreg")
})




#Check up the error functions  i.e. wrong order of censoring/ events
#rx outside of [0,1]
test_that("Errors in arm >1",{
  expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, propX+1),immdef, 
                       lowphi=-1, hiphi=1), "Invalid values for rx")
})

test_that("Errors in arm <1",{
  expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, propX-1),immdef, 
                       lowphi=-1, hiphi=1), "Invalid values for rx")
})

test_that("Errors in randomisation out of {0,1}",{
  expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm+0.5, propX-1),immdef, 
                       lowphi=-1, hiphi=1), "Invalid values for rx")
})


test_that("Censoring before Time warning",{
  expect_warning( rpsftm(ReCen(progyrs, censyrs*0.5)~Instr(imm, propX),immdef, 
                       lowphi=-1, hiphi=1), "You have observed events AFTER censoring")
})

#test_that("Too Many Recen() terms",
 #         {
  #          expect_error( rpsftm(ReCen(progyrs, censyrs)+ReCen(censyrs,progyrs)~Instr(imm, 1-xoyrs/progyrs),immdef, 
   #                              lowphi=-1, hiphi=1), "Exactly one Recen\\(\\) term needed")
    #        
     #     }
#)

form <- terms(ReCen(progyrs, censyrs)+ReCen(censyrs,progyrs)~Instr(imm, 1-xoyrs/progyrs),data=immdef,
      specials=c("ReCen","Instr")
      )
head(model.frame(form,data=immdef))


# missing data handling
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