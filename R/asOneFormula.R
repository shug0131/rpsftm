asOneFormula <- function (..., omit = c(".", "pi")) 
{
  names <- unique(allVarsRec(list(...)))
  names <- names[is.na(match(names, omit))]
  if (length(names)) 
    eval(parse(text = paste("~", paste(names, collapse = "+")))[[1]])
  else ~1
}


allVarsRec <- function (object) 
{
  if (is.list(object)) {
    unlist(lapply(object, allVarsRec))
  }
  else {
    all.vars(object)
  }
}

one_becomes_three <- function(formula){
  
  formula <- terms(formula, specials = "rand")
  var_list <- attr(formula, "variables")
  rand_index <- attr(formula, "specials")$rand
  # Do I care about the environmen, or enclosure for rand_formula, randomise, or treatment??
  rand_formula <- eval( var_list[[1+rand_index]][[2]])
  rand_formula <- terms(rand_formula)
  formula <- drop.terms(formula, dropx=rand_index-1, keep.response = TRUE)
  randomise <- delete.response(rand_formula)
  treatment <- eval(call("~", attr(rand_formula,"variables")[[2]]))
  list(formula=outcome, treatment=treatment, randomise=randomise, rand_formula=rand_formula)
}


test <- one_becomes_three(Surv(progyrs,prog)~entry+rand(I(1-xoyrs/progyrs)~imm))
test <- one_becomes_three(Surv(progyrs,prog)~entry+rand(imm+def~imm) +age/sex)
asOneFormula(test)



