get_mode <- function(x, order = 1, type = c('value', 'count')) {
  type <- match.arg(type)
  
  tab <- table(x)
  tab <- tab[order(-tab)]
  
  if (type == 'value') {
    result <- names(tab)[order]
  } else if (type == 'count') {
    result <- tab[order]
    result <- unname(result)
  }
  return(result)
}