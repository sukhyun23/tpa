# missing ratio of column
NAratio <- function(data) {
  ndata <- nrow(data)
  sapply(data, function(column) sum(is.na(column))/ndata)
}
