na_ratio <- function(data, axis = 1) {
  na_dat <- is.na(data)
  n <- nrow(data)
  p <- ncol(data)
  
  if (axis == 1) {
    result <- rowSums(na_dat)/p
  } else if (axis == 2) {
    result <- colSums(na_dat)/n
  }
  return(result)
}

remove_na <- function(data, ...) UseMethod('remove_na')

remove_na.data.frame <- function(data, axis = 1, ratio = 0.5) {
  n <- nrow(data)
  p <- ncol(data)
  NA_ratio <- na_ratio(data, axis)
  NA_idx <- which(NA_ratio >= ratio)
  NA_idx <- unname(NA_idx)
  
  if (length(NA_idx) == 0) {
    return(data)
  } 
  
  if (axis == 1) {
    data <- data[-NA_idx, ]
  } else if (axis == 2) {
    data <- data[, -NA_idx]
  }
  
  return(data)
}

remove_na.data.table <- function(data, axis = 1, ratio = 0.5) {
  n <- nrow(data)
  p <- ncol(data)
  NA_ratio <- na_ratio(data, axis)
  NA_idx <- which(NA_ratio >= ratio)
  NA_idx <- unname(NA_idx)
  
  if (length(NA_idx) == 0) {
    return(data)
  } 
  
  if (axis == 1) {
    data <- data[-NA_idx, ]
  } else if (axis == 2) {
    data <- data[, -NA_idx, with = F]
  }
  
  return(data)
}