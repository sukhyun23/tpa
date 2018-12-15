# missing ratio of column
NAratio <- function(data) {
  ndata <- nrow(data)
  sapply(data, function(column) sum(is.na(column))/ndata)
}

# remove column of row with missing values more than the ratio
RemoveNA <- function(data, axis = 1, ratio = 0.5) {
  n <- nrow(data)
  p <- ncol(data)

  if(axis == 1) { # row
    NA_ratio <- apply(data, 1, function(x) sum(is.na(x))/n)
    NA_idx <- which(NA_ratio >= ratio)
    if(length(NA_idx) == 0) {
      NA_idx <- -(1:n)
    }
    data <- data[-NA_idx, ]
  } else if(axis == 2) {
    NA_ratio <- colSums(is.na(data))/n
    NA_idx <- which(NA_ratio >= ratio)
    if(length(NA_idx) == 0) {
      NA_idx <- -(1:p)
    }
    data <- data[, -NA_idx]
  }
  return(data)
}
