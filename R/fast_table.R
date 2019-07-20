fast_table <- function(x) {
  unique_x <- sort(unique(x))
  x_t <- match(x, unique_x)
  tab <- tabulate(x_t)
  names(tab) <- unique_x
  return(tab)
}