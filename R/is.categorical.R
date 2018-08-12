is.categorical <- function(x){
  if(is.character(x) | is.ordered(x) | is.factor(x)){
    return(T)
  } else {
    return(F)
  }
}
