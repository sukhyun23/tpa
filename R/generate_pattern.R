asd
rm(list = ls())

require(dplyr)
require(data.table)
library(keras)
library(tensorflow)


# random ------------------------------------------------------------------
gen_random <- function(n) {
  uniform_random <- function(n) runif(n)
  normal_random <- function(n) rnorm(n)
  beta_random <- function(n) {
    beta_param <- seq(0.3, 1.5, by = 0.1)
    len_param <- length(beta_param)
    idx1 <- sample(len_param, 1)
    idx2 <- sample(len_param, 1)
    rbeta(n, beta_param[idx1], beta_param[idx2])
  }
  chi_random <- function(n) {
    chi_param <- sample(5, 1)
    direction <- sample(c(1, -1), 1)
    rchisq(n, df = chi_param) * direction
  }
  f <- c(uniform_random, normal_random, beta_random, chi_random)
  result <- scale(f[[sample(4, 1)]](n))
  return(result)
}

dev.off()
par(mfrow=c(3,3))
for (i in 1:9) {
  gen_random(100) %>% hist()  
}

# drifit ------------------------------------------------------------------
gen_drift <- function(x) {
  x_len <- length(x)
  std <- seq(0.1, 1.5, by = 0.1)
  
  y <- 2*x
  y <- y + rnorm(x_len, 0, std[sample(length(std), 1)])
  y <- scale(y)
  
  # y <- y-min(y)+1
  return(list(x = x, y = y))
}
x <- gen_random(100)
y <- gen_drift(x)
plot(x,y$y)



# multi -------------------------------------------------------------------
gen_multi <- function(x) {
  n_line <- sample(2:7, 1)
  x_len <- length(x)
  # ratio1 <- runif(1, 0, 1/n_line)
  y <- cut(x, n_line)
  
  value <- runif(n_line)
  levels(y) <- value
  
  y <- as.numeric(as.character(y))
  y <- y[sample(x_len, x_len)]
  
  return(list(x = x, y = scale(y)))
}
x <- gen_random(100)
y <- gen_multi(x)
plot(x,y$y)



# haunt -------------------------------------------------------------------
gen_haunt <- function(x) {
  x_len <- length(x)
  
  ratio <- runif(1, 0.001, 0.03)
  n_haunt <- x_len*ratio
  n_haunt <- ifelse(n_haunt < 1, 1, n_haunt)
  n_haunt <- ifelse(n_haunt > 15, 15, n_haunt)
  #n_haunt <- sample(1:15, 1)
  
  y <- gen_random(x_len)
  idx <- sample(x_len, n_haunt)
  
  type <- sample(3, 1) # c('up', 'low', 'both')
  width1 <- sample(3:10, 1)
  if (type == 1) {
    width2 <- sample(3:10, 1)
    y[idx] <- ifelse(y[idx] < 0, y[idx] + width1*min(y), y[idx] + width2*max(y))
  } else if (type == 2) {
    y[idx] <- y[idx] + width1*max(y)
  } else {
    y[idx] <- y[idx] + width1*min(y)
  }
  
  return(list(x = x, y = scale(y)))
}
x <- gen_random(300)
y <- gen_haunt(x)$y
plot(x, y)



# shift -------------------------------------------------------------------
x <- gen_random(300)
gen_shift <- function(x){
  x_len <- length(x)
  y <- gen_random(x_len)
  
  point <- runif(1, min = quantile(x, 0.1), max = quantile(x, 0.9))
  shift_value <- sample(c(3:8, -3:-8), 1)
  y[x >= point] <- y[x >= point] + shift_value
  
  return(list(x = x, y = scale(y)))
}
x <- gen_random(300)
y <- gen_shift(x)$y
plot(x, y)




# periodic ----------------------------------------------------------------
gen_periodic <- function(n = 100) {
  # x <- gen_random(n)
  x <- runif(n)
  n_chunk <- sample(2:6, 1)
  len_x <- length(x)
  
  case <- cut(x, n_chunk, labels = F) #/len_x
  
  x_sorted <- sort(x)
  x_sorted_list <- split(x_sorted, case)
  
  y_periodic <- unlist(x_sorted_list)
  y_periodic <- y_periodic + 
    rnorm(n, 0, runif(1, 0, 0.2))
  y_periodic <- scale(y_periodic)
  
  x_periodic <- 1:length(y_periodic)
  x_periodic <- scale(x_periodic)
  
  return(list(x = x_periodic, y = y_periodic))
}

pr <- gen_periodic(600)
pr$x %>% hist()
plot(pr$x, pr$y)





dev.off()
par(mfrow = c(2,3))
x <- gen_random(300)

y1 <- gen_drift(x)$y
y2 <- gen_haunt(x)$y
y3 <- gen_multi(x)$y
y4 <- gen_shift(x)$y
y5 <- gen_random(length(x))
y6 <- gen_periodic(300)

lapply(list(y1, y2, y3, y4, y5, y6$y), mean)

plot(x, y1, main = 'drift')
plot(x, y2, main = 'haunting')
plot(x, y3, main = 'multi-line')
plot(x, y4, main = 'shifted')
plot(x, y5, main = 'random')
plot(y6$x, y6$y, main = 'periodic')


#
x <- gen_random(n)


# non linear data check
# generate and modeling

a <- keras::dataset_cifar10()

# random 
random_list <- list()
drift_list <- list()
haunt_list <- list()
multiline_list <- list()
shifted_list <- list()
period_list <- list()
n_cand <- 30:800

for (i in 1:2000) {
  n <- sample(n_cand, 1)
  
  # random
  x <- gen_random(n)
  y <- gen_random(n)  
  random_list[[i]] <- list(x=x, y=y)
  
  # drift
  x <- gen_random(n)
  y <- gen_drift(x)$y
  drift_list[[i]] <- list(x=x, y=y)
  
  # haunt
  x <- gen_random(n)
  y <- gen_haunt(x)$y
  haunt_list[[i]] <- list(x=x, y=y)
  
  # multiline
  x <- gen_random(n)
  y <- gen_multi(x)$y
  multiline_list[[i]] <- list(x=x, y=y)
  
  # shifted
  x <- gen_random(n)
  y <- gen_shift(x)$y
  shifted_list[[i]] <- list(x=x, y=y)
  
  # periodic
  x <- gen_random(n)
  y <- gen_periodic(x)$y
  period_list[[i]] <- list(x=x, y=y)
}

data_list <- c(
  random_list, drift_list, haunt_list, 
  multiline_list, shifted_list, period_list
)
l <- c(
  'random', 'drift', 'haunt', 
  'multiline', 'shifted', 'periodic'
)
y_labels <- c(sapply(l, function(x) rep(x, 2000)))

kde2_list <- lapply(
  data_list, 
  function(o) MASS::kde2d(o$x, o$y, h = 0.25)
)

contour(kde2_list[[200]])
plot(data_list[[200]]$x, data_list[[200]]$y)

require(data.table)
d <- lapply(kde2_list, function(x) x$z %>% c())
d <- as.data.table(d)
d <- d %>% t %>% data.table()
d %>% dim()
d$y <- factor(y_labels)

tr_idx <- caret::createDataPartition(y_labels, p = 0.7)

tr_dat <- d[tr_idx$Resample1, ]
te_dat <- d[-tr_idx$Resample1, ]

tr_dat %>% dim()
te_dat %>% dim()

# install.packages('ranger')
require(ranger)
rf <- ranger(y~., data = tr_dat)
pred <- predict(rf, te_dat)

caret::confusionMatrix(
  pred$predictions, 
  te_dat$y
)


library(keras)
# keras
model <- keras_model_sequential()
model %>% 
  layer_dense(
    units = 64,
    activation = 'relu',
    input_shape = c(625)
  ) %>% 
  layer_dense(
    units = 8,
    activation = 'relu'
  ) %>% 
  layer_dense(
    units = 6,
    activation = 'softmax'
  )
summary(model)

model %>% compile(
  loss = "kullback_leibler_divergence",
  optimizer = optimizer_rmsprop(),
  metrics = c("accuracy")
)

history <- model %>% fit(
  # as.matrix(tr_dat[, 1:625]), 
  # to_categorical(as.numeric(tr_dat$y)), 
  as.matrix(tr_dat[, 1:625]),
  as.numeric(tr_dat$y),
  epochs = 30, 
  batch_size = 64, 
  validation_split = 0.2
)
pred <- predict(model, te_dat[, 1:625])
caret::confusionMatrix(pred$predictions, te_dat$y)


# boosting
# activation
# loss
# metrics
# optimizer