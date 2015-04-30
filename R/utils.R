build_formula=function(arm, adjustors=NULL){
  if(is.null(adjustors)){
    adjustors=as.formula("~1")
  }
  update_formula=paste("~.",substitute(arm),sep="+")
  update.formula(adjustors, update_formula)
}
