### Data Quality Report

# branch functions
Q1 <- function(...) quantile(x = ..., probs = 0.25)
Q3 <- function(...) quantile(x = ..., probs = 0.75)
BasicInfo <- function(x) {
  n <- length(x)
  n_na <- sum(is.na(x))
  ratio_na <- n_na/n
  card <- length(unique(x))
  ratio_card <- card/n
  
  result <- c(n = n, missing = n_na, missing.r = ratio_na,
              cardinality = card, card.r = ratio_card)
  return(result)
}

# create generic function
Summary2 <- function(x) UseMethod('Summary2')

# by class
Summary2.default <- function(x) {  
  f <- list(min, Q1, median, mean, Q3, max, sd)
  result <- vapply(X = f, FUN = function(f) f(x, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'Q1', 'median', 'mean', 'Q3', 'max', 'sd')
  return(result)
}
Summary2.Date <- function(x, origin = '1970-01-01') {  
  x_num <- as.numeric(x)
  f <- c(min, Q1, median, mean, Q3, max)
  result <- vapply(X = f, FUN = function(f) f(x_num, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'Q1', 'median', 'mean', 'Q3', 'max')
  result <- as.Date(result, origin = origin)
  return(result)
}
Summary2.POSIXct <- function(x, origin = '1970-01-01 00:00:00') {  
  x_num <- as.numeric(x)
  f <- c(min, Q1, median, mean, Q3, max)
  result <- vapply(X = f, FUN = function(f) f(x_num, na.rm = T), FUN.VALUE = c(1))
  names(result) <- c('min', 'Q1', 'median', 'mean', 'Q3', 'max')
  result <- as.POSIXct(result, origin = '1970-01-01 00:00:00')
  return(result)
}
Summary2.character <- function(x) {
  or <- c(1, 1, 2, 2)
  ty <- c('value', 'count', 'value', 'count')
  result <- unlist(Map(f = function(order, type) GetMode(x, order, type), order = or, type = ty))
  names(result) <- c('F.mode.value', 'F.mode.count', 'S.mode.value', 'S.mode.count')
  return(result)
}
Summary2.factor <- function(x) {
  x <- as.character(x)
  or <- c(1, 1, 2, 2)
  ty <- c('value', 'count', 'value', 'count')
  result <- unlist(Map(f = function(order, type) GetMode(x, order, type), order = or, type = ty))
  names(result) <- c('F.mode.value', 'F.mode.count', 'S.mode.value', 'S.mode.count')
  return(result)
}

as.data.frame.Summary2 <- function(x) {
  as.data.frame(x)  
}
Summary2.data.frame <- function(data) {
  # to apply 2 functions(BasicInfo, Summary2), create DQR class
  DQR <- function(data) {
    result <- list(BasicInfo = lapply(data, BasicInfo), 
                   Summary = lapply(data, Summary2))
    structure(result, class = 'DQR')
  }
  # summarise DQR result 
  as.data.frame.DQR <- function(dqr) {
    type <- sapply(dqr$Summary, length)
    type[type == 7] <- 'quantitative'
    type[type == 4] <- 'qualitative'
    type[type == 6] <- 'date'
    summary_split <- split(dqr$Summary, type)
    basic_split <- split(dqr$BasicInfo, type)
    
    asdata <- function(x) data.frame(t(as.data.frame(x)))
    summary_result <- lapply(summary_split, asdata)
    basic_result <- lapply(basic_split, asdata)
    
    result <- list()
    for (i in 1:length(basic_result)) {
      result[[i]] <- cbind(basic_result[[i]], summary_result[[i]])
    }
    return(result)
  }
  
  # result
  dqr <- DQR(data)
  return(as.data.frame(dqr))
}

