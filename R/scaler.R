# min max normalization ---------------------------------------------------
# rm(list = ls())
minmax_norm <- function(
  x, a = 0, b = 1, 
  mx = max(x, na.rm = T), 
  mi = min(x, na.rm = T)
) {
  x * (b-a) / (mx-mi) - (b-a)*mi/(mx-mi) + a  
}
minmax_norm_reverse <- function(x, a = 0, b = 1, mx, mi) {
  (x + (b-a)*mi/(mx-mi) - a) * (mx-mi)/(b-a)
}
normal_norm <- function(x, m, s) (x-m)/s
normal_norm_reverse <- function(x, m, s) (x*s)+m
split_col <- function(data) {
  dat_quan <- Filter(tpa::is.quan, data)
  dat_qual <- Filter(function(x) !tpa::is.quan(x), data)
  var_order <- names(data)
  
  result <- list(
    quan = dat_quan, 
    qual = dat_qual, 
    order = var_order
  )
  
  return(result)
}

# scaler ------------------------------------------------------------------
scaler <- function(data, type, ...) {
  UseMethod('scaler')  
}

scaler.data.frame <- function(
  data, 
  type = c('normal', 'minmax')
) {
  # arguments
  type <- match.arg(type)
  
  # split
  split_result <- split_col(data)
  
  # statistics
  if (type == 'normal') {
    m <- colMeans(split_result$quan)
    s <- vapply(split_result$quan, sd, na.rm = T, c(1))
    stat_list <- list(mean = m, sd = s)
  } else if (type == 'minmax') {
    mx <- vapply(split_result$quan, max, na.rm = T, c(1))
    mi <- vapply(split_result$quan, min, na.rm = T, c(1))
    stat_list <- list(min = mi, max = mx)
  }
  
  # result
  result <- structure(
    c(
      stat_list, 
      order = list(split_result$order),
      type = list(type)
    ),
    class = 'scaler'
  )
  return(result)
}

predict.scaler <- function(object, data, reverse = F) {
  data_list <- split_col(data)
  
  if (!reverse) {
    if (object$type == 'normal') {
      result <- Map(
        normal_norm, 
        data_list$quan, 
        m = object$mean, 
        s = object$sd
      )  
    } else if (object$type == 'minmax') {
      result <- Map(
        minmax_norm, 
        data_list$quan, 
        mx = object$max, 
        mi = object$min
      )  
    }
  } else if (reverse) {
    if (object$type == 'normal') {
      result <- Map(
        normal_norm_reverse, 
        data_list$quan, 
        m = object$mean, 
        s = object$sd
      )  
    } else if (object$type == 'minmax') {
      result <- Map(
        minmax_norm_reverse, 
        data_list$quan, 
        mx = object$max, 
        mi = object$min
      )  
    }
  }
  dat_quan <- as.data.frame(result)
  
  data[, names(data_list$quan)] <- dat_quan
  return(data)
}

# data <- iris
# data <- iris %>% data.table()
# 
# object <- scaler(data)
# object <- scaler(data, type = 'minmax')
# 
# d1 <- predict(object, data)
# d2 <- predict(object, d1, reverse = T)
# 
# iris$Sepal.Length == d2$Sepal.Length
# iris == d2
# d1 %>% summary()