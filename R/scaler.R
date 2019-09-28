# min max normalization ---------------------------------------------------
min_max_norm <- function(
  x, a = 0, b = 1, 
  mx = max(x, na.rm = T), 
  mi = min(x, na.rm = T)
) {
  x * (b-a) / (mx-mi) - (b-a)*mi/(mx-mi) + a  
}
min_max_norm_reverse <- function(x, a = 0, b = 1, mx, mi) {
  (x + (b-a)*mi/(mx-mi) - a) * (mx-mi)/(b-a)
}


# scaler ------------------------------------------------------------------
scaler <- function(train, test, type, ...) {
  UseMethod('scaler')  
}

scaler.data.frame <- function(
  train, test, 
  type = c('normal', 'minmax')
) {
  # arguments
  type <- match.arg(type)
  
  # split
  tr_quan <- Filter(tpa::is.quan, train)
  te_quan <- Filter(tpa::is.quan, test)
  
  tmpfun <- function(x) !tpa::is.quan(x)
  tr_qual <- Filter(tmpfun, train)
  te_qual <- Filter(tmpfun, test)
  
  names_order <- names(train)
  
  # train
  if (type == 'normal') {
    tr_result <- data.frame(scale(tr_quan))
    stats <- list(
      mean = colMeans(tr_quan),
      sd = vapply(tr_quan, sd, 1)    
    )
  } else if (type == 'minmax') {
    tr_result <- data.frame(sapply(tr_quan, min_max_norm))
    stats <- list(
      min = vapply(tr_quan, min, 1),
      max = vapply(tr_quan, max, 1)
    )
  }
  tr_result <- cbind(tr_result, tr_qual)
  tr_result <- tr_result[, names_order]
  
  # test
  if (type == 'normal') {
    te_result <- data.frame(
      Map(
        function(x, m, s) (x-m)/s,
        x = te_quan, 
        m = stats$mean, 
        s = stats$sd
      )
    )
  } else if (type == 'minmax') {
    te_result <- data.frame(
      Map(
        function(x, mx, mi) min_max_norm(x, mx = mx, mi = mi),
        x = te_quan, 
        mx = stats$max, 
        mi = stats$min
      )
    )
  }
  te_result <- cbind(te_result, te_qual)
  te_result <- te_result[, names_order]
  
  result <- list(train = tr_result, test = te_result)
  return(result)
}

scaler.data.table <- function(
  train, test,
  type = c('normal', 'minmax')
) {
  # arguments
  type <- match.arg(type)
  
  # split
  tr_quan <- Filter(tpa::is.quan, train)
  te_quan <- Filter(tpa::is.quan, test)
  
  tmpfun <- function(x) !tpa::is.quan(x)
  tr_qual <- Filter(tmpfun, train)
  te_qual <- Filter(tmpfun, test)
  
  names_order <- names(train)
  
  # train
  if (type == 'normal') {
    tr_result <- data.table(scale(tr_quan))
    stats <- list(
      mean = colMeans(tr_quan),
      sd = vapply(tr_quan, sd, 1)    
    )
  } else if (type == 'minmax') {
    tr_result <- data.table(sapply(tr_quan, min_max_norm))
    stats <- list(
      min = vapply(tr_quan, min, 1),
      max = vapply(tr_quan, max, 1)
    )
  }
  tr_result <- cbind(tr_result, tr_qual)
  tr_result <- tr_result[, names_order, with = F]
  
  # test
  if (type == 'normal') {
    te_result <- as.data.table(
      Map(
        function(x, m, s) (x-m)/s,
        x = te_quan, 
        m = stats$mean, 
        s = stats$sd
      )
    )
  } else if (type == 'minmax') {
    te_result <- as.data.table(
      Map(
        function(x, mx, mi) min_max_norm(x, mx = mx, mi = mi),
        x = te_quan, 
        mx = stats$max, 
        mi = stats$min
      )
    )
  }
  te_result <- cbind(te_result, te_qual)
  te_result <- te_result[, names_order, with = F]
  
  result <- list(train = tr_result, test = te_result)
  return(result)
}

scaler.matrix <- function(
  train, test,
  type = c('normal', 'minmax')
) {
  # arguments
  type <- match.arg(type)
  
  # train
  if (type == 'normal') {
    tr_result <- scale(train)
    stats <- list(
      mean = attr(tr_result, 'scaled:center'),
      sd = attr(tr_result, 'scaled:scale')
    )
  } else if (type == 'minmax') {
    tr_result <- apply(train, 2, min_max_norm)
    stats <- list(
      min = apply(train, 2, min),
      max = apply(train, 2, max)
    )
  }
  
  # test
  if (type == 'normal') {
    te_result <- as.data.frame(
      Map(
        function(x, m, s) (x-m)/s,
        x = as.list(data.frame(test)),
        m = stats$mean, 
        s = stats$sd
      )
    )
  } else if (type == 'minmax') {
    te_result <- as.data.frame(
      Map(
        function(x, mx, mi) min_max_norm(x, mx = mx, mi = mi),
        x = as.list(data.frame(test)),
        mx = stats$max, 
        mi = stats$min
      )
    )
  }
  te_result <- as.matrix(te_result)

  result <- list(train = tr_result, test = te_result)
  return(result)
}


# unscaler ----------------------------------------------------------------
unscaler <- function(train, test, type, stats, ...) {
  UseMethod('unscaler')
}

unscaler.data.frame <- function(
  train, test, 
  type = c('normal', 'minmax'),
  stats
) {
  # arguments
  type <- match.arg(type)
  
  # split
  tr_quan <- Filter(tpa::is.quan, train)
  te_quan <- Filter(tpa::is.quan, test)
  
  tmpfun <- function(x) !tpa::is.quan(x)
  tr_qual <- Filter(tmpfun, train)
  te_qual <- Filter(tmpfun, test)
  
  names_order <- names(train)
  
  # train
  if (type == 'normal') {
    tr_result <- data.frame(
      Map(
        function(x, m, s) x*s + m,
        x = tr_quan,
        m = stats$mean,
        s = stats$sd
      )  
    )
  } else if (type == 'minmax') {
    tr_result <- data.frame(
      Map(
        min_max_norm_reverse, 
        x = tr_quan, 
        mx = stats$max, 
        mi = stats$min    
      )
    )
  }
  tr_result <- cbind(tr_result, tr_qual)
  tr_result <- tr_result[, names_order]
  
  # test
  if (type == 'normal') {
    te_result <- data.frame(
      Map(
        function(x, m, s) x*s + m,
        x = te_quan,
        m = stats$mean,
        s = stats$sd
      )  
    )
  } else if (type == 'minmax') {
    te_result <- data.frame(
      Map(
        min_max_norm_reverse, 
        x = te_quan, 
        mx = stats$max, 
        mi = stats$min    
      )
    )
  }
  te_result <- cbind(te_result, te_qual)
  te_result <- te_result[, names_order]
  
  result <- list(train = tr_result, test = te_result)
  return(result)
}

unscaler.data.table <- function(
  train, test, 
  type = c('normal', 'minmax'),
  stats
) {
  # arguments
  type <- match.arg(type)
  
  # split
  tr_quan <- Filter(tpa::is.quan, train)
  te_quan <- Filter(tpa::is.quan, test)
  
  tmpfun <- function(x) !tpa::is.quan(x)
  tr_qual <- Filter(tmpfun, train)
  te_qual <- Filter(tmpfun, test)
  
  names_order <- names(train)
  
  # train
  if (type == 'normal') {
    tr_result <- as.data.table(
      Map(
        function(x, m, s) x*s + m,
        x = tr_quan,
        m = stats$mean,
        s = stats$sd
      )  
    )
  } else if (type == 'minmax') {
    tr_result <- as.data.table(
      Map(
        min_max_norm_reverse, 
        x = tr_quan, 
        mx = stats$max, 
        mi = stats$min    
      )
    )
  }
  tr_result <- cbind(tr_result, tr_qual)
  tr_result <- tr_result[, names_order, with = F]
  
  # test
  if (type == 'normal') {
    te_result <- as.data.table(
      Map(
        function(x, m, s) x*s + m,
        x = te_quan,
        m = stats$mean,
        s = stats$sd
      )  
    )
  } else if (type == 'minmax') {
    te_result <- as.data.table(
      Map(
        min_max_norm_reverse, 
        x = te_quan, 
        mx = stats$max, 
        mi = stats$min    
      )
    )
  }
  te_result <- cbind(te_result, te_qual)
  te_result <- te_result[, names_order, with = F]
  
  result <- list(train = tr_result, test = te_result)
  return(result)
}

# set.seed(1)
# idx <- sample(100, 100)
# train_raw <- iris[idx, ] %>% data.table()
# test_raw <- iris[-idx, ] %>% data.table()
# a <- scaler(train_raw, test_raw, type = 'minmax')
# train <- a$train
# test <- a$test
# 
# type <- 'minmax'
# stats <- list(
#   max = sapply(train_raw[, 1:4], max),
#   min = sapply(train_raw[, 1:4], min)
# )
# 
# train_raw
# a <- unscaler(train, test, 'minmax', stats)
# train_raw$Sepal.Length == a$train$Sepal.Length
# test_raw$Sepal.Length == a$test$Sepal.Length