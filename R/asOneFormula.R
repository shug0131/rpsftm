



add_formula_list <- function(formula_list, env=parent.frame()){
  lhs_var_list <- lapply(formula_list, lhs)
  rhs_var_list <- lapply(formula_list, rhs)
  lhs_text <- paste(unlist(lhs_var_list), collapse="+")
  rhs_text <- paste(unlist(rhs_var_list), collapse="+")
  formula <- as.formula(paste(lhs_text, "~",rhs_text))
  environment(formula) <- env
  terms(formula, simplify=TRUE)
}




lhs <- function(formula){
  formula <- as.formula(formula)
  if(length(formula)==3){ 
    paste(deparse(formula[[2]]),collapse="")
  } else{ character(0) }
}

rhs <- function(formula){
  formula <- as.formula(formula)
  if(length(formula)==3){
    paste(deparse(formula[[3]]), collapse="")
  } else{ 
    paste(deparse(formula[[2]]), collapse="")
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
    # Converting from the old syntax with a warning for back compatibility
    old_expression <- rand_expression
    rand_expression <- substitute(rand(.treat~.arm),  list(.treat=rand_expression[[3]],.arm=rand_expression[[2]]))
    warning("'",deparse(old_expression), "' is deprecated syntax. This is converted to '", 
            deparse(rand_expression),"'", call. = FALSE
            )
    
    
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
  if(length(attr(rand_formula,"variables"))<=2){warning("your argument to rand() makes very little sense")}
  randomise <- delete.response(rand_formula)
  treatment <- eval(call("~", attr(rand_formula,"variables")[[2]]))
  environment(treatment) <- env
  list(formula=formula, treatment=terms(treatment), randomise=terms(randomise))
}


#test <- one_becomes_three(Surv(progyrs,prog)~entry+rand(I(1-xoyrs/progyrs)~imm))
#test <- one_becomes_three(Surv(progyrs,prog)~entry+rand(imm+def~imm) +age/sex)
#one_becomes_three(y~x+rand(a~x)+rand(x~a))

#asOneFormula(test)



