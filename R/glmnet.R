cv_glmnet_coef <- function(object, s = 'lambda.min', intercept = F, nonzero = F) {
  co <- coef(object, s)
  co <- as.matrix(co)
  v <- rownames(co)
  co <- c(co)
  names(co) <- v
  
  if (!intercept) {
    co <- co[-1]
  }
  
  if (nonzero) {
    co <- co[co != 0]
  }
  
  return(co)
}
