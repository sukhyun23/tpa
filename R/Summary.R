Summary <- function(data){
  Card <- function(x) length(table(x))
  NofNA <- function(x) sum(is.na(x))
  Q1 <- function(x, na.rm=T) quantile(x, 0.25, na.rm = na.rm)
  Q3 <- function(x, na.rm=T) quantile(x, 0.75, na.rm = na.rm)
  BasicInfo <- function(x){
    funs <- c(length, NofNA, Card)
    result <- sapply(funs, function(f) f(x))
    result[4] <- result[2]/result[1]
    result[5] <- result[3]/result[1]
    names(result) <- c('n', 'Miss', 'Card', 'Miss_R', 'Card_R')
    return(result)
  }

  # error
  reserved_words <- c("if", "else", "while", "function", "for",
                      "in", "next", "break", " TRUE", "FALSE", "NULL", "Inf",
                      "NaN", "NA")
  if (sum(names(data) %in% reserved_words) != 0) {
    stop("변수명은 예약어로 사용할 수 없습니다.\n 예약어 : if, else, while,\n
         function, for, in, next, break, TRUE, FALSE, NULL, Inf, NaN, NA")
  }

  # idx
  date_idx <- sapply(data, is.date)
  num_idx <- sapply(data, is.numerical)
  char_idx <- sapply(data, is.categorical)

  # data
  date_dat <- data[date_idx]
  for(i in names(date_dat[sapply(date_dat, lubridate::is.Date)])){
    date_dat[[i]] <- as.POSIXct(date_dat[[i]])
  }
  num_dat <- data[num_idx]
  char_dat <- data[char_idx]

  # calculating
  # date
  if(ncol(date_dat)==0){
    date_result <- NA
  } else {
    date_result <- cbind(
      data.frame(t(sapply(date_dat, BasicInfo))),
      data.frame(t(sapply(date_dat, summary)))
    )
    names(date_result)[6:11] <- c('Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max.')
    for(i in 6:11) date_result[[i]] <- as.POSIXct(date_result[[i]], origin='1970-01-01')
  }

  # numerical
  if(ncol(num_dat)==0){
    num_result <- NA
  } else {
    num_result <- cbind(t(sapply(num_dat, BasicInfo)), t(sapply(num_dat, summary)))
  }

  # categorical
  if(ncol(char_dat)==0){
    char_result <- NA
  } else {
    mode_dat <- cbind(
      sapply(char_dat, GetMode, order=1, type='value'),
      sapply(char_dat, GetMode, order=1, type='count'),
      sapply(char_dat, GetMode, order=2, type='value'),
      sapply(char_dat, GetMode, order=2, type='count')
    )
    char_result <- cbind(data.frame(t(sapply(char_dat, BasicInfo))),
                         data.frame(mode_dat, stringsAsFactors = F))
    names(char_result)[6:9] <- c('FMode', 'FMode_C', 'SMode', 'SMode_C')
    char_result$FMode_C <- as.numeric(char_result$FMode_C)
    char_result$SMode_C <- as.numeric(char_result$SMode_C)
    char_result$FMode_R <- char_result$FMode_C/char_result$n
    char_result$SMode_R <- char_result$SMode_C/char_result$n
  }
  char_result <- char_result[c(1:7, 10, 8, 9, 11)]

  return(list(num_result, char_result, date_result))
}
