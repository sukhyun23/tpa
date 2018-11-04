# class function
# numeric
is.numerical <- function(x){
  crit <- class(x)
  if (length(crit) >= 2) {
    crit <- paste(crit, collapse = " ")
  }
  try(if (crit %in% c("numeric", "integer"))
    return(TRUE)
    else return(FALSE), silent = T)
}

# categorical
is.categorical <- function(x){
  if(is.character(x) | is.ordered(x) | is.factor(x)){
    return(T)
  } else {
    return(F)
  }
}

# data
is.date <- function(x) inherits(x, 'Date') || inherits(x, 'POSIXct')
