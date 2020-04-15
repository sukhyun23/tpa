is.quan <- function(x) {
  class_x <- class(x)
  result <- any(class_x %in% c('numeric', 'integer'))
}

is.qual <- function(x) {
  class_x <- class(x)
  result <- any(class_x %in% c('character', 'factor', 'ordered'))
}

is.date <- function(x) {
  class_x <- class(x)
  result <- any(class_x %in% c('Date', 'POSIXct', 'POSIXlt', 'POSIXt'))
}

class2 <- function(x) {
  if (is.quan(x)) return('quantitative')
  if (is.qual(x)) return('qualitative')
  if (is.date(x)) return('date')
}