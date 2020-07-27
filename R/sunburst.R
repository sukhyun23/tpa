sunburst <- function(...) UseMethod('sunburst')

sunburst.data.frame <- function(data, vars, center) {
  # hierarchical frequency table 
  recurse <- function(data, vars, parents = NULL, result = list()) {
    if (length(vars) == 0) {
      return(result)
    } else {
      v <- vars[1]
      if (length(parents) == 0) {
        tab <- list(table(data[,v]))
        tab <- list(tab); names(tab) <- v
      } else {
        dat_list <- split(data, data[,parents])
        tab <- lapply(dat_list, function(d,v) table(d[,v]), v=v)
        tab <- list(tab)
        names(tab) <- v
      }
      parents <- c(parents, v)
      result <- append(result, tab)
    }
    recurse(data, vars[-1], parents, result = result)
  }
  tab_list <- recurse(data, vars)
  
  # table to data.frame for plotly form 
  result_list <- list()
  for (i in seq_along(tab_list)) {
    if (i == 1) {
      labels <- unlist(lapply(tab_list[[i]], names))
      ids <- labels
      parents <- rep('', length(ids))
      values <- unname(unlist(tab_list[[i]]))
      
      if (!missing(center)) {
        labels <- c(center, labels)
        ids <- c(center, ids)
        parents <- c('', rep(center, length(parents)))
        values <- c(sum(values), values)
      }
      
      df <- data.frame(
        ids=ids, labels=labels, parents=parents, values=values, 
        stringsAsFactors = F
      )
    } else {
      labels <- lapply(tab_list[[i]], names)
      ids <- unname(unlist(Map(paste, names(labels), labels, sep = '-')))
      ids <- gsub('\\.', '-', ids)
      labels <- unname(unlist(labels))
      
      parents <- names(tab_list[[i]])
      parents <- unlist(Map(rep, parents, sapply(tab_list[[i]], length)))
      parents <- unname(parents)
      parents <- gsub('\\.', '-', parents)
      
      values <- unname(unlist(tab_list[[i]]))
      
      df <- data.frame(
        ids=ids, labels=labels, parents=parents, values=values, 
        stringsAsFactors = F
      )
    }
    result_list[[i]] <- df
  }
  result <- do.call(rbind, result_list)
  result <- structure(result, class = 'sunburst_df')
  return(result)
}

sunburst.sunburst_df <- function(
  object, branchvalues = c('remainder', 'total')
) {
  branchvalues <- match.arg(branchvalues)
  
  p <- plotly::plot_ly(
    ids = object$ids,
    labels = object$labels,
    parents = object$parents,
    values = object$values,
    branchvalues = branchvalues, 
    type = 'sunburst',
  )
  return(p)
}

# data <- ggplot2::diamonds
# data$cut <- gsub(' ', '', data$cut)
# vars <- c('cut', 'color', 'clarity')
# object <- sunburst(data, vars, center = 'data')
# sunburst(object)
