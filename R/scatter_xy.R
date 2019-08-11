scatter_xy <- function(data, y, cor.filter = NA, sort = T) {
  y_data <- data[[y]]
  
  data <- Filter(tpa::is.quan, data)  
  x_data <- data[, which(names(data) != y)]
  x_data <- x_data[, which(vapply(x_data, sd, c(1)) != 0)]
  
  cors <- vapply(x_data, function(x) cor(x, y_data, use = 'complete'), c(1))
  if (!is.na(cor.filter)) {
    cor_idx <- which(abs(cors) >= cor.filter)
    x_data <- x_data[, cor_idx]
    cors <- cors[cor_idx]
  }
  
  if (sort) {
    cor_idx <- order(abs(cors), decreasing = T)
    x_data <- x_data[, cor_idx]
    cors <- cors[cor_idx]
  }
  
  sc_plot <- function(x, title, x_title) {
    plot(
      x, 
      y_data, 
      main = title, 
      xlab = x_title, 
      ylab = y, 
      pch = 19
    )
  }
  
  invisible(
    Map(
      sc_plot, 
      x = x_data, 
      title = round(cors, 3), 
      x_title = names(cors)
    )
  )
}