# Data Quality Report
# test commit
# create s3 class
DQR <- function(x) UseMethod('DQR')


# default function
DQR.default <- function(data){
  # functions
  Card <- function(x) length(table(x))
  NofNA <- function(x) sum(is.na(x))
  Q1 <- function(x, na.rm=T) quantile(x, 0.25, na.rm = na.rm)
  Q3 <- function(x, na.rm=T) quantile(x, 0.75, na.rm = na.rm)
  BasicInfo <- function(x, summary = F){
    funs <- c(length, NofNA, Card)
    result <- sapply(funs, function(f) f(x))
    result[4] <- result[2]/result[1]
    result[5] <- result[3]/result[1]

    if (summary == T) {
      result <- c(result, as.numeric(summary(x))[1:6])
      names(result) <- c('n', 'Miss', 'Card', 'Miss_R', 'Card_R',
                         'Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max')
      return(result)
    } else {
      names(result) <- c('n', 'Miss', 'Card', 'Miss_R', 'Card_R')
      return(result)
    }
  }

  # error
  reserved_words <- c("if", "else", "while", "function", "for",
                      "in", "next", "break", " TRUE", "FALSE", "NULL", "Inf",
                      "NaN", "NA")
  if (sum(names(data) %in% reserved_words) != 0){
    stop("변수명은 예약어로 사용할 수 없습니다.\n 예약어 : if, else, while,\n
         function, for, in, next, break, TRUE, FALSE, NULL, Inf, NaN, NA")
  }

  # data
  date_dat <- Filter(f = is.date, x = data)
  num_dat <- Filter(f = is.numerical, x = data)
  char_dat <- Filter(f = is.categorical, x = data)

  # calculating
  # date
  if (ncol(date_dat) == 0) {
    date_result <- NA
  } else {
    for(i in names(date_dat)){
      date_dat[[i]] <- as.POSIXct(date_dat[[i]])
    }
    tmp_list <- lapply(date_dat, summary)
    date_result <- cbind(
      data.frame(t(sapply(date_dat, BasicInfo))),
      t(data.frame(as.character(tmp_list$date)))
    )
    names(date_result)[6:11] <- c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max')
    # for(i in 6:11) date_result[[i]] <- as.POSIXct(date_result[[i]], origin='1970-01-01')
  }

  # numerical
  if (ncol(num_dat) == 0) {
    num_result <- NA
  } else {
    num_result <- data.frame(t(sapply(num_dat, BasicInfo, summary = T)))
  }

  # categorical
  if (ncol(char_dat) == 0) {
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
    char_result <- data.frame(char_result[c(1:7, 10, 8, 9, 11)])
  }

  result <- list(numerical = num_result, categorical = char_result, date = date_result)
  result <- structure(result, class = 'DQR')
  return(result)
}


# data.table class
DQR.data.table <- function(data) {
  # functions
  Card <- function(x) length(na.omit(unique(x)))
  NofNA <- function(x) sum(is.na(x))
  Q1 <- function(x, na.rm=T) quantile(x, 0.25, na.rm = na.rm)
  Q3 <- function(x, na.rm=T) quantile(x, 0.75, na.rm = na.rm)
  BasicInfo <- function(x, summary = F){
    funs <- c(length, NofNA, Card)
    result <- sapply(funs, function(f) f(x))
    result[4] <- result[2]/result[1]
    result[5] <- result[3]/result[1]

    if (summary == T) {
      result <- c(result, as.numeric(summary(x))[1:6])
      names(result) <- c('n', 'Miss', 'Card', 'Miss_R', 'Card_R',
                         'Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max')
      return(result)
    } else {
      names(result) <- c('n', 'Miss', 'Card', 'Miss_R', 'Card_R')
      return(result)
    }
  }

  # error
  reserved_words <- c("if", "else", "while", "function", "for",
                      "in", "next", "break", " TRUE", "FALSE", "NULL", "Inf",
                      "NaN", "NA")
  if (sum(names(data) %in% reserved_words) != 0) {
    stop("변수명은 예약어로 사용할 수 없습니다.\n 예약어 : if, else, while,\n
         function, for, in, next, break, TRUE, FALSE, NULL, Inf, NaN, NA")
  }

  # data
  date_dat <- Filter(f = is.date, x = data)
  num_dat <- Filter(f = is.numerical, x = data)
  char_dat <- Filter(f = is.categorical, x = data)

  # calculating
  # date
  if (ncol(date_dat) == 0) {
    date_result <- NA
  } else {
    for(i in names(date_dat)){
      date_dat[[i]] <- as.POSIXct(date_dat[[i]])
    }
    tmp_list <- lapply(date_dat, summary)
    date_result <- cbind(
      data.frame(t(sapply(date_dat, BasicInfo))),
      t(data.frame(as.character(tmp_list$date)))
    )
    names(date_result)[6:11] <- c('Min', 'Q1', 'Median', 'Mean', 'Q3', 'Max')
    # for(i in 6:11) date_result[[i]] <- as.POSIXct(date_result[[i]], origin='1970-01-01')
  }

  # numerical
  if (ncol(num_dat) == 0) {
    num_result <- NA
  } else {
    num_result <- data.frame(t(sapply(num_dat, BasicInfo, summary = T)))
  }

  # categorical
  if (ncol(char_dat) == 0) {
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
    char_result <- data.frame(char_result[c(1:7, 10, 8, 9, 11)])
  }

  result <- list(numerical = num_result, categorical = char_result, date = date_result)
  result <- structure(result, class = 'DQR')
  return(result)
}


plot.DQR <- function(DQR) {
  for(i in c('numerical', 'categorical', 'date')){
    DQR[[i]]$Variable <- rownames(DQR[[i]])
    DQR[[i]]$type <- i
  }

  gdat <- do.call(rbind, lapply(DQR, function(data) data[c('Variable', 'Miss_R', 'type')]))
  bar_missing <- geom_bar(aes(x=reorder(Variable, -gdat$Miss_R), y=Miss_R, fill=type), stat='identity')
  scale_y <- scale_y_continuous(limits = c(0,1), breaks = c(0, 0.25, 0.5, 0.75, 1),
                                labels = c('0', '25', '50', '75', '100'))
  scale_fill <- scale_fill_manual(values = c('dodgerblue4', 'deeppink4', 'gray50'))
  xylab <- labs(x='Variables', y='Missing Ratio(%)')
  theme1 <- theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
                  panel.background = element_rect(fill='white'),
                  panel.grid.major.y = element_line(color='gray75'),
                  axis.ticks = element_blank())
  ggplot(gdat) +
    bar_missing +
    scale_y +
    scale_fill +
    xylab +
    theme1
}


