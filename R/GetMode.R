GetMode <- function(x, mode_order = ..., type = c("value", "count")) {
  # arguments setting
  len_x <- length(x)
  mode_order <- match(mode_order, 1:len_x)
  type <- match.arg(type, choices = c("value", "count"), several.ok = F)
  
  # calculation table
  count_tab <- sort(table(x), decreasing = T)
  mode_count <- count_tab
  mode_value <- names(count_tab)
  
  # exception error
  if (is.null(mode_count) || is.null(mode_value)) {
    return(NA)
  }
  
  if (type == 'value') {
    return(mode_value[mode_order])
  } else if (type == 'count') {
    return(unname(mode_count[mode_order]))
  }
}