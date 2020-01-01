# # random ------------------------------------------------------------------
# gen_random <- function(n, ...) {
#   uniform_random <- function(n) runif(n)
#   normal_random <- function(n) rnorm(n)
#   beta_random <- function(n) {
#     beta_param <- seq(0.3, 1.5, by = 0.1)
#     len_param <- length(beta_param)
#     idx1 <- sample(len_param, 1)
#     idx2 <- sample(len_param, 1)
#     rbeta(n, beta_param[idx1], beta_param[idx2])
#   }
#   chi_random <- function(n) {
#     chi_param <- sample(5, 1)
#     direction <- sample(c(1, -1), 1)
#     rchisq(n, df = chi_param) * direction
#   }
#   f <- c(uniform_random, normal_random, beta_random, chi_random)
#   result <- scale(f[[sample(4, 1)]](n))
#   return(result)
# }
# 
# 
# # drifit ------------------------------------------------------------------
# gen_drift <- function(x, ...) {
#   x_len <- length(x)
#   std <- seq(0.1, 1.5, by = 0.1)
#   
#   y <- sample(c(2,-2), 1)*x
#   y <- y + rnorm(x_len, 0, std[sample(length(std), 1)])
#   y <- scale(y)
#   
#   # y <- y-min(y)+1
#   return(list(x = x, y = y))
# }
# 
# 
# # multi -------------------------------------------------------------------
# gen_multi <- function(n, ...) {
#   if (n < 200) n <- 200
#   x <- gen_random(n)
#   
#   n_line <- sample(2:7, 1)
#   x_len <- length(x)
#   # ratio1 <- runif(1, 0, 1/n_line)
#   y <- cut(x, n_line)
#   
#   value <- runif(n_line)
#   levels(y) <- value
#   
#   y <- as.numeric(as.character(y))
#   y <- y[sample(x_len, x_len)]
#   
#   return(list(x = x, y = scale(y)))
# }
# 
# 
# 
# # haunt -------------------------------------------------------------------
# gen_haunt <- function(x, ...) {
#   x_len <- length(x)
#   
#   ratio <- runif(1, 0.001, 0.03)
#   n_haunt <- x_len*ratio
#   n_haunt <- ifelse(n_haunt < 1, 1, n_haunt)
#   n_haunt <- ifelse(n_haunt > 15, 15, n_haunt)
#   #n_haunt <- sample(1:15, 1)
#   
#   y <- gen_random(x_len)
#   idx <- sample(x_len, n_haunt)
#   
#   type <- sample(3, 1) # c('up', 'low', 'both')
#   width1 <- sample(3:10, 1)
#   if (type == 1) {
#     width2 <- sample(3:10, 1)
#     y[idx] <- ifelse(y[idx] < 0, y[idx] + width1*min(y), y[idx] + width2*max(y))
#   } else if (type == 2) {
#     y[idx] <- y[idx] + width1*max(y)
#   } else {
#     y[idx] <- y[idx] + width1*min(y)
#   }
#   
#   return(list(x = x, y = scale(y)))
# }
# 
# 
# 
# # shift -------------------------------------------------------------------
# gen_shift <- function(x, ...) {
#   x_len <- length(x)
#   y <- gen_random(x_len)
#   
#   point <- runif(1, min = quantile(x, 0.1), max = quantile(x, 0.9))
#   shift_value <- sample(c(3:8, -3:-8), 1)
#   y[x >= point] <- y[x >= point] + shift_value
#   
#   return(list(x = x, y = scale(y)))
# }
# 
# 
# 
# 
# # periodic ----------------------------------------------------------------
# gen_periodic <- function(n = 100, ...) {
#   # x <- gen_random(n)
#   x <- runif(n)
#   n_chunk <- sample(2:6, 1)
#   len_x <- length(x)
#   
#   case <- cut(x, n_chunk, labels = F) #/len_x
#   
#   x_sorted <- sort(x)
#   x_sorted_list <- split(x_sorted, case)
#   
#   y_periodic <- unlist(x_sorted_list)
#   y_periodic <- y_periodic + 
#     rnorm(n, 0, runif(1, 0, 0.2))
#   y_periodic <- scale(y_periodic)
#   
#   x_periodic <- 1:length(y_periodic)
#   x_periodic <- sample(c(-1,1), 1)*scale(x_periodic)
#   
#   return(list(x = x_periodic, y = y_periodic))
# }
# 
# 
# 
# # generate data -----------------------------------------------------------
# generate <- function(
#   pattern = c('drift', 'haunt', 'shift', 'multi', 'periodic', 'random')
# ) {
#   tmp_gen_random <- function(n) {
#     list(x = gen_random(n), y = gen_random(n))
#   }
#   
#   # function
#   pattern <- match.arg(pattern)
#   f <- switch (pattern,
#                'drift' = gen_drift,
#                'haunt' = gen_haunt,
#                'shift' = gen_shift,
#                'multi' = gen_multi,
#                'periodic' = gen_periodic,
#                'random' = tmp_gen_random
#   )
#   
#   # arguments
#   n <- sample(50:1000, 1)
#   if (pattern %in% c('drift', 'haunt', 'shift')) {
#     x <- gen_random(n)
#     args <- list(n = n, x = x)
#   } else {
#     args <- list(n = n)
#   }
#   do.call(f, args)
# }