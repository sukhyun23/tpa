NASyntax <- function(data, NAstring){
  for(i in names(data)){
    data[[i]][which(data[[i]] %in% NAstring)] <- NA
  }
  message('The object must be created.')
  invisible(data)
}
