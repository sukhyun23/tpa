# create s3 class
RemoveNA <- function(x) UseMethod('RemoveNA')

# remove column of row with missing values more than the ratio
RemoveNA.default <- function(data, axis = 1, ratio = 0.5) {
  n <- nrow(data)
  p <- ncol(data)

  if(axis == 1) { # row
    NA_ratio <- apply(data, 1, function(x) sum(is.na(x))/p)
    NA_idx <- which(NA_ratio >= ratio)
    if(length(NA_idx) == 0) {
      NA_idx <- -(1:n)
    }
    data <- data[-NA_idx, ]
  } else if(axis == 2) { # column
    NA_ratio <- colSums(is.na(data))/n
    NA_idx <- which(NA_ratio >= ratio)
    if(length(NA_idx) == 0) {
      NA_idx <- -(1:p)
    }
    data <- data[, -NA_idx]
  }
  return(data)
}


# data.table class
RemoveNA.data.table <- function(data, axis = 1, ratio = 0.5) {
  n <- nrow(data)
  p <- ncol(data)

  if(axis == 1) { # row
    NA_ratio <- apply(data, 1, function(x) sum(is.na(x))/p)
    NA_idx <- which(NA_ratio >= ratio)
    if(length(NA_idx) == 0) {
      NA_idx <- -(1:n)
    }
    data <- data[-NA_idx, ]
  } else if(axis == 2) { # column
    NA_ratio <- colSums(is.na(data))/n
    NA_idx <- which(NA_ratio >= ratio)
    if(length(NA_idx) == 0) {
      NA_idx <- -(1:p)
    }
    data <- data[, -NA_idx, with = F]
  }
  return(data)
}


