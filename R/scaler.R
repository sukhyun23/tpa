scaler <- function(data, m, s, mean.sd = F, ...) UseMethod('scaler')

scaler.matrix <- function(data, m = NULL, s = NULL, mean.sd = F) {
  if (is.null(m) & is.null(s)) {
    m <- colMeans(data)
    s <- apply(data, 2, sd)
  }
  
  data <- scale(data)
  
  if (mean.sd) {
    result <- list(data = data, m = m, s = s)
    return(result)
  } else if (!mean.sd) {
    return(data)
  }
}

scaler.data.frame <- function(data, m = NULL, s = NULL, mean.sd = F) {
  data <- Filter(tpa::is.quan, data)
  
  if (is.null(m) & is.null(s)) {
    m <- colMeans(data)
    s <- vapply(data, sd, c(1))
  }
  
  sc <- function(x, m, s) (x-m)/s
  data <- as.data.frame(Map(sc, x = data, m = m, s = s))
  
  if (mean.sd) {
    result <- list(data = data, m = m, s = s)
    return(result)
  } else if (!mean.sd) {
    return(data)
  }
}

scaler.data.table <- function(data, m = NULL, s = NULL, mean.sd = F) {
  data <- Filter(tpa::is.quan, data)
  
  if (is.null(m) & is.null(s)) {
    m <- colMeans(data)
    s <- vapply(data, sd, c(1))
  }
  
  sc <- function(x, m, s) (x-m)/s
  data <- as.data.table(Map(sc, x = data, m = m, s = s))
  
  if (mean.sd) {
    result <- list(data = data, m = m, s = s)
    return(result)
  } else if (!mean.sd) {
    return(data)
  }
}
