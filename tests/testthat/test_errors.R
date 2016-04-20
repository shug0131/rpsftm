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

test_that("Too Many Recen() terms",
         {
          expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(imm, 1-xoyrs/progyrs)+ReCen(censyrs,progyrs),immdef, 
                              lowphi=-1, hiphi=1), "Recen\\(\\) term only on the LHS of the formula")
        
     }
)

test_that("No Instr() terms",
          {
            expect_error( rpsftm(ReCen(progyrs, censyrs)~imm,immdef, 
                                 lowphi=-1, hiphi=1), "Exactly one Instr\\(\\) term allowed")
            
          }
)

test_that("Too many Instr() terms",
          {
            expect_error( rpsftm(ReCen(progyrs, censyrs)~def*Instr(imm, 1-xoyrs/progyrs),immdef, 
                                 lowphi=-1, hiphi=1), "Exactly one Instr\\(\\) term allowed")
            
          }
)

test_that("Instr() interaction",
          {
            expect_error( rpsftm(ReCen(progyrs, censyrs)~def/Instr(imm, 1-xoyrs/progyrs),immdef, 
                                 lowphi=-1, hiphi=1), "Instr\\(\\) term must not be in any interactions")
            
          }
)



test_that("More than 2 arms",
          {
            myarm <- factor(rep(1:4,250))
            
            expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(myarm, 1-xoyrs/progyrs),immdef, 
                                 lowphi=-1, hiphi=1), "arm must have exactly 2 observed values")
          }
)

test_that("less than 2 arms",
          {
            
            
            expect_error( rpsftm(ReCen(progyrs, censyrs)~Instr(1, 1-xoyrs/progyrs),immdef, 
                                 lowphi=-1, hiphi=1), "arm must have exactly 2 observed values")
          }
)

rpsftm(ReCen(progyrs, censyrs)~as.factor(immdef$def)/Instr(imm, 1-xoyrs/progyrs),immdef, 
       lowphi=-1, hiphi=1)
form <- terms(ReCen(progyrs, censyrs)~as.factor(immdef$def==1)/Instr(imm, 1-xoyrs/progyrs),data=immdef,
      specials=c("ReCen","Instr")
      )
Instr_index <- attr(form,"specials")$Instr
Instr_drops=which(attr(form,"factors")[Instr_index,]>0)

head(model.matrix(terms(progyrs~as.factor(def)/xoyrs, data=immdef),data=immdef)


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