library(data.table)
library(reshape2)
library(dplyr)


profile_ttest <- function(value, group, raw_value) {
  # simple t-test (equal variance)
  
  # error
  gnames <- unique(group)
  if (length(gnames) <= 1) {
    return(NA)
  }
  
  # mean, variance
  ns <- table(group)
  ns <- c(ns[gnames])
  
  vars <- tapply(value, group, var)
  vars <- ifelse(is.na(vars), 0, vars)
  vars <- vars[gnames]
  
  means <- tapply(value, group, mean)
  means <- means[gnames]
  
  # small data / pooled variance
  if (max(ns) < 10) {
    df <- 10
    sp <- sd(raw_value)
  } else {
    df <- length(value)-2
    sp <- sqrt(sum((ns-1)*vars) / df)
  }
  
  # tstat, p-value, t distribution
  tstat <- (means[1] - means[2]) / (sp*sqrt(sum(1/ns)))
  
  pvalue <- pt(tstat, df = df, lower.tail = F)
  pvalue <- min(c(pvalue*2, (1-pvalue)*2))
  
  return(pvalue)
}

# value <- c(rnorm(30), rnorm(5, 1, 1)) 
# group <- c(rep('A', 30), rep('B', 5))
# raw_value <- value
# 
# t.test(value ~ group, var.equal = T)
# profile_ttest(value, group, raw_value)

profile_plot <- function(data) {
  dat_ref <- data[group == 'ref', ]
  dat_tar <- data[group == 'tar', ]
  dat_mean <- data[, .(m = mean(value)), by = 'seq']
  g1 <- ggplot(dat_ref) + 
    geom_line(
      aes(x = seq, y = value, group = id, color = group),
      color = 'grey80'
    ) + 
    geom_line(
      aes(x = seq, y = m), color = 'dodgerblue4', 
      data = dat_mean, size = 1.5, alpha = 1
    ) + 
    geom_line(
      aes(x = seq, y = value), color = 'deeppink4', 
      data = dat_tar, size = 1.5, alpha = 1
    ) + 
    xlab('Sequence') + ylab('Value') + 
    scale_x_continuous(breaks = 1:10) + 
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color = 'grey90'),
      panel.grid.major.y = element_line(color = 'grey90')
    )
  return(g1)
}

# # dat
# m <- c(10, 12, 16, 13, 13, 10, 13.5, 13, 10, 11)
# s <- c(1, 0.5, 0.7, 0.8, 0.3, 1, 0.3, 1.2, 1.1, 1.5)
# tdat <- Map(function(m, s) rnorm(40, m, s), m = m, s = s) %>% as.data.table()
# 
# dat <- melt(tdat, variable.name = 'seq', variable.factor = T)
# dat$seq <- dat$seq %>% as.numeric()
# dat$group <- 'ref'
# dat$id <- rep(1:40, 10)
# 
# # normal
# tar1 <- c(10, 12, 16, 13, 13, 10, 13.5, 13, 10, 11) + rnorm(10, 0, 0.4)
# 
# # level
# tar1 <- c(10, 12, 16, 13, 13, 10, 13.5, 13, 10, 11) + rnorm(10, 0, 0.3) + 3
# 
# # parallel
# tar1 <- c(10, 12, 16, 13, 13, 10, 13.5, 10, 13.5, 9.5) + rnorm(10, 0, 0.3)
# 
# # tar1 <- c(10.5, 12, 14.5, 12, 12.5, 10, 13, 12.5, 11, 11.5)
# 
# dat <- rbind(
#   dat, 
#   data.table(seq = 1:10, value = tar1, group = 'tar', id = 41)  
# )
# 
# # 
# dev.off()
# profile_plot(dat) + ggtitle('Outlier (not parallel)')
# 
# list(nor, lev, para)
# 
# profile_level(dat$value, dat$seq, dat$group)
# 
# 
# dat_ref <- dat[group == 'ref']
# dat_tar <- dat[group == 'tar']
# tmp_list <- list()
# for (i in 1:10) {
#   tmp_val <- dat_ref[seq == i, ]$value
#   xd <- density(tmp_val, bw = 1.8)
#   
#   x <- minmax_norm(xd$y, a = i, b = i+0.8)
#   y <- minmax_norm(xd$x, a = min(tmp_val), b = max(tmp_val))
#   tmp_list[[i]] <- geom_line(
#     aes(x = x, y = y),
#     alpha = 0.4, 
#     data = data.table(x = x, y = y)
#   )
# }
# g1 <- profile_plot(dat) + tmp_list + 
#   geom_point(
#     aes(x = seq+0.15, y = value), 
#     shape = '-', size = 25, 
#     color = 'deeppink4',
#     data = dat_tar
#   )
# 
# g1
  

  
  
  



tmp_list[[1]]


minmax_norm <- function(x, a = 0, b = 1, mx = max(x), mi = min(x)) {
  x * (b - a)/(mx - mi) - (b - a) * mi/(mx - mi) + a
}

# geom_point(aes(x = value, y = 0))



profile_level <- function(value, seq, group) {
  # t.test(value ~ group) by sequence
  result <- Map(
    function(v, g) profile_ttest(v, g, value),
    v = split(value, seq),
    g = split(group, seq)
  )
  
  return(result)
}


parallel <- function(list) {
  dt <- list
  
  t_s <- dt$group[dt$target == 'tar']
  r_s <- dt$group[dt$target == 'ref']
  
  if (uni_len(t_s) != uni_len(r_s)) {
    inter <- dplyr::intersect(r_s, t_s)
    if (length(inter) <= 2) {
      return(NA)
    }
    dt <- dt[group %in% inter, ]
  }
  
  dtt <- dcast(dt, id + target ~ group, value.var = 'value')
  dtt <- impute_mean(dtt, 1)
  cont_mat <- contrast_matrix(dtt)
  
  p_score <- c()
  for (i in 1:ncol(cont_mat)) {
    p_score[i] <- bsl_ttest(cont_mat[, i], dtt$target, dt$value)
  }
  return(p_score)
  
}

contrast_matrix <- function(x) {
  x <- Filter(is.quan, x)
  x <- as.matrix(x)
  
  ##
  if (ncol(x) == 1) {
    x <- t(x)
  }
  ##
  
  # dimension
  p <- ncol(x)
  n <- nrow(x)
  
  # contrast matrix
  cmat <- matrix(0, nrow = p, ncol = p-1)
  mat_idx <- Map(function(x, y) c(x, y), x = 1:(p-1), y = (2:p))
  
  for (i in 1:ncol(cmat)) {
    cmat[mat_idx[[i]], i] <- c(1, -1)
  }
  
  yset <- x %*% cmat
  return(yset)
  
}