Impurity <- function(x, impurity.measure = c("gini")){
  tab <- table(x)
  p_tab <- prop.table(x)
  if (impurity.measure == "deviance") {
    p_tab[p_tab == 0] <- 1
    impurity <- -2 * sum(p_tab * log(p_tab))
  }
  else if (impurity.measure == "gini") {
    impurity <- 1 - sum(p_tab^2)
  }
  return(impurity)
}
