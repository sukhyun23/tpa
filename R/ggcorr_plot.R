ggCorrPlot <- function(mat, order = c('aoe', 'fpc', 'hclust', 'alpha'),
                       text = T, text.col=c('white', 'black'), text.size = 4.5,
                       axis.text.size = 10, title = '', title.size = 20,
                       legend.height = 10,
                       col = c('red', 'blue'), na.col = 'grey45',
                       diag = F, range = c(-1,1)) {
  # order of variables
  r_aoe <- function(mat) {
    x.eigen <- eigen(mat)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    e2 <- x.eigen[, 2]
    alpha <- ifelse(e1 > 0, atan(e2 / e1), atan(e2 / e1) + pi)
    order(alpha) # returned vector
  }
  r_fpc <- function(mat) {
    x.eigen <- eigen(mat)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    order(e1) # returned vector
  }
  r_hclust <- function(mat, hclust.method = 'complete') {
    hc <- hclust(as.dist(1 - mat), method = hclust.method)
    order.dendrogram(as.dendrogram(hc)) # returned vector
  }
  r_alpha <- function(mat) {
    order(colnames(mat))
  }

  # sorting by the order
  order <- match.arg(order)
  order <- switch(
    order,
    'aoe' = r_aoe(mat),
    'fpc' = r_fpc(mat),
    'hclust' = r_hclust(mat),
    'alpha' = r_alpha(mat)
  )
  mat <- mat[order, order]

  # remove duplicated values
  lower_idx <- lower.tri(mat)
  mat[lower_idx] <- 0
  gdat <- reshape2::melt(mat)

  #
  if(nrow(gdat) == 0) {
    return(NA)
  } else {
    # text color
    gdat$text.col <- ifelse(c(lower_idx), '0', '1')
    if(diag == F) {
      # diag of matrix
      gdat[gdat$Var1==gdat$Var2,]$value <- NA
    }
    gdat$text.col <- ifelse(abs(gdat$value)>=0.6, '0', gdat$text.col)

    # correlation text
    gdat$text_value <- gdat$value
    gdat[lower_idx,]$text_value <- NA
    if(length(text.col) == 1) {
      text.col <- rep(text.col, 2)
    }
    cor_text <- ggplot2::geom_text(ggplot2::aes(x=Var1, y=Var2,
                                                label = round(text_value,2),
                              color = text.col), size=text.size, fontface = 'bold')
    cor_text_option1 <- ggplot2::scale_color_manual(values = text.col)
    if(text == F) {
      cor_text <- NULL
      cor_text_option1 <- NULL
    }

    # plot
    g1 <- ggplot2::ggplot(gdat) +
      ggplot2::geom_tile(ggplot2::aes(x=Var1, y=Var2, fill=value)) +
      cor_text + cor_text_option1 +
      ggplot2::scale_fill_gradient2(low = col[1], high=col[2], midpoint = 0,
                                    na.value = na.col, limits = range) +
      ggplot2::guides(color= F,
                      fill = ggplot2::guide_colorbar(title = '',
                                                     barwidth = 1,
                                                     barheight = legend.height,
                                                     ticks =F)) +

      ggplot2::ggtitle(label = title) +
      ggplot2::labs(x='', y='') +
      ggplot2::theme(
        axis.text = ggplot2::element_text(size = axis.text.size),
        axis.text.x = ggplot2::element_text(angle=90, vjust=0.35, hjust=1),
        panel.background = ggplot2::element_rect(fill = 'white'),
        plot.title = ggplot2::element_text(size = title.size)
      )
    return(g1)
  }
}
