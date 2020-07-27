sankey <- function(...) UseMethod('sankey')

sankey.data.frame <- function(data, vars) {
  # variables order
  len_vars <- length(vars)
  idx1 <- 1:(len_vars-1)
  idx2 <- idx1+1
  idx_list <- Map(function(x,y) c(x,y), idx1, idx2)
  vars_list <- lapply(idx_list, function(idx, x) x[idx], x = vars)
  
  # duplicated category
  vars_unique <- lapply(data[, vars], unique)
  is_dup <- list()
  for (i in vars) {
    v <- vars_unique[[i]]
    comp <- vars_unique[-which(vars == i)]
    isu <- sapply(comp, function(x, v) any(x %in% v), v = v)
    if (any(isu)) {
      is_dup[[i]] <- names(isu)[isu]
    }
  }
  is_dup <- unique(unlist(is_dup))
  
  # frequency table
  freqtab <- function(d, v, is_dup) {
    dsub <- d[,v]
    result <- aggregate(rep(1, nrow(dsub)), dsub, sum)
    names(result) <- c('source', 'target', 'value')
    result$source <- as.character(result$source)
    result$target <- as.character(result$target)
    result
  }
  
  # if duplicated category, add variable name
  if (length(is_dup) >= 1) {
    for (i in is_dup) data[[i]] <- paste(i, data[[i]], sep = '.')
  }
  tab_list <- lapply(vars_list, freqtab, d = data)
  
  # sankey data for plotly
  sankey_dat <- do.call(rbind, tab_list)
  sankey_dat$label_source <- sankey_dat$source
  sankey_dat$label_target <- sankey_dat$target
  
  label <- unique(c(sankey_dat$source, sankey_dat$target))
  sankey_dat$source <- as.numeric(factor(sankey_dat$source, label))-1
  sankey_dat$target <- as.numeric(factor(sankey_dat$target, label))-1
  
  result <- list(sankey_dat = sankey_dat, label = label, tab_list = tab_list)
  result <- structure(result, class = 'sankey_df')
  return(result)
}

sankey.sankey_df <- function(object, palette = 'Set1', tables = FALSE, title = '') {
  # info
  sankey_dat <- object$sankey_dat
  label <- object$label
  tab_list <- object$tab_list
  
  # color
  color <- suppressWarnings(RColorBrewer::brewer.pal(length(label), palette))
  color <- color[1:length(label)]
  
  is_na_col <- is.na(color)
  while (any(is_na_col)) {
    na_col_len <- sum(is_na_col)
    suppressWarnings(
      color[is_na_col] <- RColorBrewer::brewer.pal(na_col_len, palette)
    )
    is_na_col <- is.na(color)
  }
  
  # plotly
  p <- plotly::plot_ly(
    type = "sankey",
    orientation = "h",
    
    node = list(
      label = label,
      color = color,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    link = list(
      source = sankey_dat$source,
      target = sankey_dat$target,
      value =  sankey_dat$value
    )
  )
  p <- plotly::layout(p = p, title = title, font = list(size = 10))
  
  if (tables) {
    return(list(p = p, tab_list = tab_list))
  }
  return(p)
}

data <- ggplot2::diamonds
data$cut <- gsub(' ', '', data$cut)
vars <- c('cut', 'color', 'clarity')
object <- sankey(data, vars)
sankey(object)


# dat <- data.table::fread('/home/sukhyun/dataset/campus/Placement_Data_Full_Class.csv')
# data <- data.frame(dat)
# vars <- c('gender', 'ssc_b', 'hsc_b', 'hsc_s', 'degree_t', 'workex', 'specialisation', 'status')
# object <- sankey(data = data, vars = vars)
# sankey(object, tables = T)