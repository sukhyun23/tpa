# require(dplyr)
# require(rpart)
# fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
# fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
#               parms = list(prior = c(.65,.35), split = "information"))
# fit3 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
#               control = rpart.control(cp = 0.05))
# rp <- fit
# y <- kyphosis$Kyphosis %>% unique %>% sort

ggRpartPlot <- function(rp, y) {
  # tm.text.col
  # br.text.col
  # br.line.col
  # class.col
  # tm.text.size
  # br.text.size

  ndata <- rp$splits[1,1]
  nclass <- length(unique(rp$y))

  # extract information of terminal nodes
  tm_frame <- rp$frame[rp$frame$var == '<leaf>', ] # terminal information data frame
  tm_n <- tm_frame$n # number of observations in each terminal node
  tm_lab <- tm_frame$yval # label(class) in each terminal node
  tm_info <- tm_frame$yval2 # information of terminal nodes
  tm_n_ratio <- tm_info[, ncol(tm_info)] # ratio of observations in each terminal node


  # create dendro data for gg
  glist <- ggdendro::dendro_data(rp)
  glist <- glist[-4] # remove uselessness
  MMTrans <- function(x, a, b, mx, mi) {
    return(x * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a)
  }
  mx <- max(glist$segments$y, na.rm = T)
  mi <- min(glist$segments$y, na.rm = T)
  for (i in 1:length(glist)) {
    glist[[i]]$y <- MMTrans(x = glist[[i]]$y, a = 0.1, b = 0.55,
                            mx = mx, mi = mi)
    if(is.null(glist[[i]]$yend)) next
    glist[[i]]$yend <- MMTrans(x = glist[[i]]$yend, a = 0.1,
                               b = 0.55, mx = mx, mi = mi)
  } # apply
  tm_plot_frame <- glist$leaf_labels

  # processing for text to be printed of terminal nodes
  tm_info <- tm_info[, c(-1,-ncol(tm_info))] # remove uselessness
  colnames(tm_info) <- c(paste('class_n', 1:nclass, sep = '_'),
                         paste('class_ratio', 1:nclass, sep = '_')) # to avoid confusion
  tm_txt_class_n <- tm_info[, 1:(ncol(tm_info)/2)] # a text to be printed on the plot
  tm_txt_class_n <- apply(tm_txt_class_n, 1, function(x) paste(x, collapse = ' / ')) # pooling
  tm_txt_class_ratio <- tm_info[, -(1:(ncol(tm_info)/2))] # a text to be printed on the plot
  tm_txt_class_ratio <- apply(tm_txt_class_ratio, 2, function(x) round(x,2)) # rounding
  tm_txt_class_ratio <- apply(tm_txt_class_ratio, 1, function(x) paste(x, collapse = ' / ')) # pooling
  tm_txt_total_ratio <- paste(tm_n, ndata, sep = ' / ') # a text to be printed on the plot
  tm_txt_total_ratio <- paste(tm_txt_total_ratio, ' (', round(tm_n_ratio, 2)*100, '%)', sep = '') # a text to be printed on the plot
  tm_txt_class_name <- as.character(tm_plot_frame$label)
  tm_txt_final <- paste(tm_txt_class_name, tm_txt_total_ratio,
                        tm_txt_class_n,  tm_txt_class_ratio, sep = '\n')
  tm_img_alpha <- apply(tm_info[, -(1:(ncol(tm_info)/2))], 1, max)
  tm_plot_frame$text <- tm_txt_final
  tm_plot_frame$alpha <- tm_img_alpha
  glist$leaf_labels$text <- tm_txt_final
  glist$leaf_labels$alpha <- tm_img_alpha
  # glist$leaf_labels$label <- as.character(glist$leaf_labels$label)


  # annotation (path)
  emt <- paste(rep(' ', nchar(as.character(glist$labels[1, ]$label))*1.5),
               collapse = '')
  yesno <- paste('yes', emt, 'no', collapse = '')

  # data frame for barplot of terminal nodes
  bar_dat <- glist$leaf_labels
  bar_dat$tm_txt_class_ratio <- tm_txt_class_ratio
  bar_dat$tm_txt_class_ratio <- lapply(stringr::str_split(bar_dat$tm_txt_class_ratio, '/'), as.numeric)
  mx <- max(unlist(bar_dat$tm_txt_class_ratio))
  mi <- min(unlist(bar_dat$tm_txt_class_ratio))
  bar_dat$tm_txt_class_ratio <- lapply(
    bar_dat$tm_txt_class_ratio,
    function(x) MMTrans(x, a = 0, b = 0.04, mx = mx, mi = mi))
  bar_dat <- data.frame(bar_dat$tm_txt_class_ratio)
  names(bar_dat) <- paste('x', 1:ncol(bar_dat), sep = '')
  if (length(unique(glist$leaf_labels$label)) != nclass) {
    bar_dat$label <- y
  } else {
    bar_dat$label <- unique(glist$leaf_labels$label)
  }
  bar_dat <- reshape2::melt(bar_dat, 'label', variable.name = 'x')
  bar_dat$x <- as.numeric(bar_dat$x)
  # bar_dat$label <- as.character(bar_dat$label)

  # drawing plot
  ggplot2::ggplot(glist$segments) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y,
                                       xend = xend, yend = yend),
                          color='gray45') +
    ggplot2::geom_text(ggplot2::aes(x=x, y=y+0.01, label = label),
                       fontface = 'bold', size=3.5, data = glist$labels) +
    ggplot2::annotate(geom='text', x=glist$labels[1,]$x,
                      y=glist$labels[1,]$y - 0.0075, label = yesno) +
    ggplot2::geom_label(ggplot2::aes(x=x, y=y-0.02, label = text,
                                     fill = label, alpha = alpha),
                        fontface = 'bold', size=4, data = glist$leaf_labels) +
    ggplot2::geom_bar(ggplot2::aes(x = x, y = value, fill = label),
                      stat='identity', data = bar_dat,
                      position ='dodge', width = 0.5) +
    # ggplot2::scale_fill_manual(values = c('red', 'blue')) +
    ggplot2::guides(alpha=F) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
}
