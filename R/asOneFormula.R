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