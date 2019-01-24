ggRpartPlot <- function(rp) {
  ndata <- rp$splits[1,1]
  nclass <- length(unique(rp$y))

  # information of terminaal leaves
  terminal_leaf <- rp$frame[rp$frame$var == '<leaf>', ]
  # number
  terminal_leaf_ni <- terminal_leaf$n
  # class
  terminal_leaf_label <- terminal_leaf$yval
  # ratio
  terminal_leaf_info <- terminal_leaf$yval2
  terminal_leaf_tratio <- terminal_leaf_info[, ncol(terminal_leaf_info)]

  # etc
  terminal_leaf_info <- terminal_leaf_info[,c(-1,-ncol(terminal_leaf_info))]
  terminal_leaf_info_count <- terminal_leaf_info[, 1:(ncol(terminal_leaf_info)/2)]
  terminal_leaf_info_count <- apply(terminal_leaf_info_count, 1, function(x) paste(x, collapse = ' / '))
  terminal_leaf_info_ratio <- terminal_leaf_info[, -(1:(ncol(terminal_leaf_info)/2))]
  terminal_leaf_info_ratio <- apply(terminal_leaf_info_ratio, 2, function(x) round(x,2))
  terminal_leaf_info_ratio <- apply(terminal_leaf_info_ratio, 1, function(x) paste(x, collapse = ' / '))

  # create dendro data for gg
  glist <- ggdendro::dendro_data(rp)
  a <- 0.1; b <- 0.55
  mx <- max(glist$segment$y)
  mi <- min(glist$segment$y)
  glist$segments$y <- glist$segment$y * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  glist$segments$yend <- glist$segment$yend * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  glist$labels$y <- glist$label$y * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  glist$leaf_labels$y <- glist$leaf_labels$y * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  terminal_info <- glist$leaf_labels
  terminal_leaf_info <- glist$leaf_labels

  # add terminal node label
  terminal_leaf_info$ni <- paste(terminal_leaf_ni, ndata, sep = ' / ')
  terminal_leaf_info$ni <- paste(terminal_leaf_info$ni, ' (', round(terminal_leaf_tratio, 2)*100, '%)', sep = '')
  terminal_leaf_info$count <- terminal_leaf_info_count
  terminal_leaf_info$ratio <- terminal_leaf_info_ratio
  terminal_leaf_info$text <- paste(terminal_leaf_info$label, terminal_leaf_info$ni, terminal_leaf_info$count, terminal_leaf_info$ratio, sep ='\n')
  terminal_leaf_info$alpha <- terminal_leaf_info$ratio %>% str_split('/') %>% sapply(function(x) as.numeric(x) %>% max)

  # annotation (path)
  emt <- paste(rep(' ', nchar(glist$labels[1, ]$label %>% as.character)*1.5),
               collapse = '')
  yesno <- paste('yes', emt, 'no', collapse = '')

  # bar plot data
  a <- 0; b <- 0.04
  bar_dat <- terminal_leaf_info[, c('x', 'label', 'ratio')]
  bar_dat$ratio <- lapply(str_split(bar_dat$ratio, '/'), as.numeric)
  mx <- max(unlist(bar_dat$ratio)); mi <- min(unlist(bar_dat$ratio))
  bar_dat$ratio <- bar_dat$ratio %>% lapply(function(x) x*(b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a)
  bar_dat <- data.frame(bar_dat$ratio)
  names(bar_dat) <- paste('x', 1:ncol(bar_dat), sep = '')
  bar_dat$label <- unique(terminal_leaf_info$label)
  bar_dat <- reshape2::melt(bar_dat, 'label', variable.name = 'x')
  bar_dat$x <- as.numeric(bar_dat$x)

  ggplot2::ggplot(glist$segments) +
    ggplot2::geom_segment(ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
                          color='gray45') +
    ggplot2::geom_text(ggplot2::aes(x=x, y=y+0.01, label = label),
                       fontface = 'bold', size=3.5, data = glist$labels) +
    ggplot2::annotate(geom='text', x=glist$labels[1,]$x,
                      y=glist$labels[1,]$y - 0.0075, label = yesno) +
    ggplot2::geom_label(ggplot2::aes(x=x, y=y-0.02, label = text,
                                     fill=label, alpha = alpha),
                        fontface = 'bold', size=4, data = terminal_leaf_info) +
    ggplot2::guides(alpha=F) +
    ggplot2::geom_bar(ggplot2::aes(x = x, y = value, fill = label),
                      stat='identity', data = bar_dat,
                      position ='dodge', width = 0.5) +
    ggplot2::theme(
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank()
    )
}
