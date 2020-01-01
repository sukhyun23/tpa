
q1 <- function(...) quantile(x = ..., probs = 0.25)
q3 <- function(...) quantile(x = ..., probs = 0.75)

# create generic function
summary2 <- function(x) UseMethod('summary2')

# by class
summary2.integer <- function(x) {  
  f <- list(min, q1, median, mean, q3, max, sd)
  result <- vapply(X = f, FUN = function(f) f(x, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'q1', 'median', 'mean', 'q3', 'max', 'sd')
  return(result)
}
summary2.numeric <- function(x) {  
  f <- list(min, q1, median, mean, q3, max, sd)
  result <- vapply(X = f, FUN = function(f) f(x, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'q1', 'median', 'mean', 'q3', 'max', 'sd')
  return(result)
}
summary2.default <- function(x) {  
  f <- list(min, q1, median, mean, q3, max, sd)
  result <- vapply(X = f, FUN = function(f) f(x, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'q1', 'median', 'mean', 'q3', 'max', 'sd')
  return(result)
}
summary2.Date <- function(x, origin = '1970-01-01') {  
  x_num <- as.numeric(x)
  f <- c(min, q1, median, mean, q3, max)
  result <- vapply(X = f, FUN = function(f) f(x_num, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'q1', 'median', 'mean', 'q3', 'max')
  result <- as.Date(result, origin = origin)
  return(result)
}
summary2.POSIXct <- function(x, origin = '1970-01-01 00:00:00') {  
  x_num <- as.numeric(x)
  f <- c(min, q1, median, mean, q3, max)
  result <- vapply(X = f, FUN = function(f) f(x_num, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'q1', 'median', 'mean', 'q3', 'max')
  result <- as.POSIXct(result, origin = '1970-01-01 00:00:00')
  return(result)
}
summary2.character <- function(x) {
  or <- c(1, 1, 2, 2)
  ty <- c('value', 'count', 'value', 'count')
  result <- unlist(Map(f = function(order, type) get_mode(x, order, type), order = or, type = ty))
  names(result) <- c('F.mode.value', 'F.mode.count', 'S.mode.value', 'S.mode.count')
  return(result)
}
summary2.factor <- function(x) {
  x <- as.character(x)
  or <- c(1, 1, 2, 2)
  ty <- c('value', 'count', 'value', 'count')
  result <- unlist(Map(f = function(order, type) get_mode(x, order, type), order = or, type = ty))
  names(result) <- c('F.mode.value', 'F.mode.count', 'S.mode.value', 'S.mode.count')
  return(result)
}
summary2.data.frame <- function(data) {
  trans <- function(data) {
    data.frame(t(data.frame(data)))
  }
  basic_info <- function(x) {
    n <- length(x)
    n_na <- sum(is.na(x))
    ratio_na <- n_na/n
    card <- length(unique(x))
    ratio_card <- card/n
    
    result <- c(
      n = n, missing = n_na, missing.r = ratio_na,
      cardinality = card, card.r = ratio_card
    )
    return(result)
  }
  
  data_list <- list(
    quantitative = Filter(is.quan, data),
    qualitative = Filter(is.qual, data),
    date = Filter(is.date, data)
  )
  data_list <- data_list[vapply(data_list, ncol, c(1)) != 0]
  names_list <- lapply(data_list, function(x) names(x))
  
  result <- list()
  for (i in names(data_list)) {
    result[[i]] <- cbind(
      trans(lapply(data_list[[i]], basic_info)),
      trans(lapply(data_list[[i]], summary2))
    )
  }
  
  return(result)
}



