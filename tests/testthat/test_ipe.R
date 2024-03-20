
full_immdef <- immdef
n <- 200
set.seed(1355)
immdef <- immdef[sample(1:nrow(immdef),size=n),]

test_that("first basict fit with mixed data sources",{
  propX <- with(immdef,1-xoyrs/progyrs)
  fit <- ipe(Surv(progyrs, prog)~rand(imm,propX),data=immdef, censor_time=censyrs)
  expect_is(fit$psi, "numeric")
})