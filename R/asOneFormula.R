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

one_becomes_three <- function(formula, env=parent.frame()){
  formula <- terms(formula, specials = "rand")
  var_list <- attr(formula, "variables")
  rand_index <- attr(formula, "specials")$rand
  if( length(rand_index)>1){ 
    stop("there can be only one rand() term in the formula")
  }
  rand_expression <- var_list[[1+rand_index]]
  if( length(rand_expression)!=2 ){
    stop("'",deparse(rand_expression), "' must have only one argument, which is a  formula")
  }
  if(!grepl("~",deparse(rand_expression[[2]]))){
    warning("'",deparse(rand_expression[[2]]), "' does not look like a formula")
  }
  rand_formula <- eval( rand_expression[[2]])
  environment(rand_formula) <- env
  rand_formula <- terms(rand_formula)
  if(!attr(rand_formula,"response")){
    stop("'", deparse(rand_expression[[2]]),"' must be a two-sided formula" )
  }
  length(attr(formula, "variables"))
  #formula <- drop.terms(formula, dropx=rand_index-1, keep.response = TRUE)
  formula <- formula[-(rand_index-1)] #this works if there is only the rand() terms
  randomise <- delete.response(rand_formula)
  treatment <- eval(call("~", attr(rand_formula,"variables")[[2]]))
  environment(treatment) <- env
  list(formula=formula, treatment=terms(treatment), randomise=randomise)
}


#test <- one_becomes_three(Surv(progyrs,prog)~entry+rand(I(1-xoyrs/progyrs)~imm))
#test <- one_becomes_three(Surv(progyrs,prog)~entry+rand(imm+def~imm) +age/sex)
#one_becomes_three(y~x+rand(a~x)+rand(x~a))

#asOneFormula(test)



