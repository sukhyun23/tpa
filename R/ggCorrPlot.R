ggcorr_plot <- function(
  mat, 
  order = c('aoe', 'fpc', 'hclust', 'alpha'),
  diag = F,
  range = c(-1,1), 
  tile.col = c('red', 'blue'), 
  tile.col.midpoint = 0,
  text = T, # a text in each tile
  text.col=c('white', 'black'), 
  text.col.level = 0.6, 
  text.size = 4.5,
  axis.text = T,
  axis.text.size = 10, 
  title = '', 
  title.size = 20, 
  na.col = 'grey45',
  legend.height = 10,
  direction = c('lower', 'upper')
) {
  # function to calculate the order of variables
  r_aoe <- function(mat) {
    x.eigen <- eigen(mat)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    e2 <- x.eigen[, 2]
    alpha <- ifelse(e1 > 0, atan(e2 / e1), atan(e2 / e1) + pi)
    return(order(alpha)) # returned vector
  }
  r_fpc <- function(mat) {
    x.eigen <- eigen(mat)$vectors[, 1:2]
    e1 <- x.eigen[, 1]
    return(order(e1)) # returned vector
  }
  r_hclust <- function(mat, hclust.method = 'complete') {
    hc <- hclust(as.dist(1 - mat), method = hclust.method)
    return(order.dendrogram(as.dendrogram(hc))) # returned vector
  }
  r_alpha <- function(mat) {
    return(order(colnames(mat)))
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
  direction <- match.arg(
    direction, 
    choices = c('lower', 'upper'), 
    several.ok = F
  )
  if (direction == 'lower') {
    tmp_idx <- lower.tri(mat)
  } else if (direction == 'upper') {
    tmp_idx <- upper.tri(mat)
  }
  mat[tmp_idx] <- 0  
  
  # cor matrix to data.frame for ggplot drawing
  gdat <- reshape2::melt(mat)
  
  # error message
  if (nrow(gdat) == 0) {
    stop('No data')
  } 
  
  # create ggplot text object and adjust options
  if (text == F) {
    cor_text <- NULL
    cor_text_option1 <- NULL
  } else if (text == T) {
    # 1. add flag variable of text color 
    gdat$text.col <- ifelse(c(tmp_idx), '0', '1') # 0 : text.col[1], 1 : text.col[2]
    if (diag == F) {
      # remove diag values of matrix
      gdat[gdat$Var1==gdat$Var2,]$value <- NA
    }
    gdat$text.col <- ifelse(abs(gdat$value) >= text.col.level, '0', gdat$text.col)
    
    # 2. correlation text
    gdat$text_value <- gdat$value
    # remove duplicated values
    gdat[tmp_idx,]$text_value <- NA
    # if text.col has only one value
    if (length(text.col) == 1) {
      text.col <- rep(text.col, 2)
    }
    # create ggplot object
    cor_text <- geom_text(
      aes(
        x = Var1, y = Var2, 
        label = round(text_value, 2), 
        color = text.col
      ),
      size = text.size, 
      fontface = 'bold', 
      na.rm = T
    )
    cor_text_option1 <- scale_color_manual(values = text.col)  
  }
  
  # axis text
  if (axis.text == F) {
    axis_text <- element_blank()
    axis_text_x <- NULL
    axis_ticks <- element_blank()
  } else if (axis.text == T) {
    axis_text <- element_text(size = axis.text.size)
    axis_text_x <- element_text(angle = 90, vjust = 0.35, hjust = 1)
    axis_ticks <- element_line()
  }
  
  # plot
  g1 <- 
    ggplot(gdat) +
    geom_tile(aes(x = Var1, y = Var2, fill = value)) +
    scale_fill_gradient2(
      low = tile.col[1],
      high = tile.col[2], 
      midpoint = tile.col.midpoint,
      na.value = na.col, 
      limits = range
    ) +
    cor_text + # geom_text
    cor_text_option1 + # scale_color_manual
    guides(
      color= F, 
      fill = guide_colorbar(
        title = '', 
        barwidth = 1, 
        barheight = legend.height, 
        ticks = F
      )
    ) +
    ggtitle(label = title) +
    labs(x = '', y = '') +
    theme(
      axis.text = axis_text,
      axis.text.x = axis_text_x,
      axis.ticks = axis_ticks,
      panel.background = element_rect(fill = 'white'),
      plot.title = element_text(size = title.size)
    )
  return(g1)
}
