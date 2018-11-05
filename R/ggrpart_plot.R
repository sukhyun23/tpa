# binary decision tree
ggrpart_plot <- function(rpart_model) {
  # extract the terminal node information from rpart object
  ndata <- rpart_model$splits[1,1]
  rp_path <- path.rpart(rpart_model, node = as.numeric(rownames(rpart_model$frame[rpart_model$frame$var == '<leaf>',])))
  terminal_leaf_ni <- rpart_model$frame[rpart_model$frame$var == '<leaf>',]$n
  terminal_leaf_label <- rpart_model$frame[rpart_model$frame$var == '<leaf>',]$yval2[,1]-1
  terminal_leaf_label_ratio <- rpart_model$frame[rpart_model$frame$var == '<leaf>',]$yval2[,5]
  terminal_leaf_ratio <- rpart_model$frame[rpart_model$frame$var == '<leaf>',]$yval2[,6]

  # create dendro data for ggplot
  glist <- ggdendro::dendro_data(rpart_model)
  a <- 0.5; b <- 1
  mx <- max(glist$segment$y)
  mi <- min(glist$segment$y)
  glist$segments$y <- glist$segment$y * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  glist$segments$yend <- glist$segment$yend * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  glist$labels$y <- glist$label$y * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  glist$leaf_labels$y <- glist$leaf_labels$y * (b-a)/(mx-mi) - (b-a)*mi/(mx-mi) + a
  terminal_info <- glist$leaf_labels

  # add terminal node label
  terminal_info$ni <- paste(terminal_leaf_ni, ndata, sep='/')
  terminal_info$fill <- as.numeric(terminal_leaf_label_ratio)
  terminal_info$ratio <- paste(round(terminal_leaf_label_ratio, 2)*100, '%')
  terminal_info$t_ratio <- paste(round(terminal_leaf_ratio, 2)*100, '%')
  terminal_info$n_leaf <- paste(terminal_info$ni, '(', terminal_info$t_ratio, ')', sep = '')
  terminal_info$ratio <- paste(terminal_info$label, terminal_info$ratio, sep='\n')
  terminal_info$text_color <- ifelse(terminal_info$fill >= 0.8|terminal_info$fill <=0.2, '1', '0')

  # add terminal node bar plot
  abNorm <- function(x,a,b) (b-a)*(x-min(x))/(max(x)-min(x))+a
  gdat <- terminal_info[,c('x', 'label', 'fill')]
  gdat$y <- gdat$fill
  gdat$y_mid <- abNorm(c(0,1,gdat$y), 0.4, 0.45)[c(-1, -2)]
  gdat$y1 <- 0.4
  gdat$y2 <- 0.45

  # plot
  ggplot(glist$segments) +
    geom_segment(aes(x=x, y=y, xend=xend, yend=yend), color='gray45') +
    geom_text(aes(x=x, y=y+0.015, label=label), fontface='bold', size=3.5, data=glist$labels) +
    geom_text(aes(x=x, y=y-0.005, label=n_leaf), data=terminal_info) +
    geom_label(aes(x=x, y=y-0.03, label=ratio, fill=fill, color=text_color), fontface='bold', size=4, data=terminal_info) +
    scale_y_continuous(limits=c(0.4, 1.015)) +
    scale_color_manual(values=c('black', 'white')) +
    guides(color = F, fill = guide_colorbar(barwidth = 0.5, barheight = 8, ticks = F)) +
    scale_fill_gradient2(low = 'firebrick1', high = 'dodgerblue4', mid = 'white',
                         midpoint = 0.5, limits = c(0,1), name = 'y value\n',
                         labels = c('0%', '25%', '50%', '75%', '100%')) +
    geom_segment(aes(x=x, xend=x, y=y1, yend=y_mid), color='dodgerblue4', size=15, data=gdat) +
    geom_segment(aes(x=x, xend=x, y=y_mid, yend=y2), color='firebrick1', size=15, data=gdat) +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank()
    )
}
