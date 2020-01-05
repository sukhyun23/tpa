impurity <- function(x, type = c('gini', 'deviance')){
  type <- match.arg(type)
  
  tab <- table(x)
  p_tab <- prop.table(tab)
  
  if (type == "deviance") {
    p_tab[p_tab == 0] <- 1
    result <- -2 * sum(p_tab * log(p_tab))
  }
  else if (type == "gini") {
    result <- 1 - sum(p_tab^2)
  }
  return(result)
}