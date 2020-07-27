surface <- function(object, ...) UseMethod('surface')

surface.gam <- function(object, view, n.grid = 30) {
  # arguments
  varnames <- names(object$var.summary)
  if (missing(view)) {
    view <- varnames[1:2]
  }
  others <- varnames[!varnames %in% view]
  viewclass <- sapply(object$var.summary[view], class)
  
  # others, central tendency
  # qualitative : mode, quantitative : median
  center <- function(x) {
    if (length(x) >= 2) return(x[2]) else return(x[1])
  }
  design_others <- as.data.frame(lapply(object$var.summary[others], center))
  
  # view, making grid matrix
  # qualitative : all, quantitative : expand min to max as many as n.grid
  expand <- function(x) {
    if (class(x) %in% c('integer', 'numeric')) {
      return(seq(x[1], x[3], length.out = n.grid)) # min, max
    } else { # (class(x) %in% c('factor', 'character'))
      return(x)
    }
  }
  
  grid <- lapply(object$var.summary[view], expand)
  v1 <- grid[[view[1]]]
  v2 <- grid[[view[2]]]
  
  design <- do.call(expand.grid, grid)
  if (nrow(design_others) >= 1) {
    design <- cbind(design, design_others)  
  }
  design$lp <- predict(object = object, newdata = design) # linear predictor
  z <- t(matrix(design$lp, ncol = n.grid, nrow = n.grid))
  
  # matrix
  plotly_obj <- plotly::plot_ly(x = v1, y = v2) 
  plotly_obj <- plotly::add_surface(p = plotly_obj, z = z)
  plotly_obj <- plotly::layout(
    p = plotly_obj,
    scene = list(
      xaxis = list(title = view[1]), 
      yaxis = list(title = view[2]),
      zaxis = list(title = 'linear predictor')
    )
  )
  return(plotly_obj)
}
