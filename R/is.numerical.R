is.numerical <- function(x){
  crit <- class(x)
  if (length(crit) >= 2) {
    crit <- paste(crit, collapse = " ")
  }
  try(if (crit %in% c("numeric", "integer"))
    return(TRUE)
    else return(FALSE), silent = T)
}
