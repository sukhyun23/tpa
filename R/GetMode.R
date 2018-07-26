GetMode <- function(x, order = 1, type = "value"){
  count_tab <- sort(table(x), decreasing = T)
  mode_count <- count_tab[order]
  value_tab <- names(count_tab)
  if (is.null(value_tab))
    value_tab <- NA
  mode_value <- value_tab[order]
  if (type == "value") {
    result <- mode_value
  }
  else if (type == "count") {
    result <- mode_count
  }
  names(result) <- NULL
  return(result)
}
