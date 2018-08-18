FindSimilarString <- function(x, FR = 0.2, ignore.case = T){
  # if(!require(ggplot2)){
  #   install.packages("ggplot2")
  #   library(ggplot2)
  # }
  x <- as.character(x)
  if (any(nchar(na.omit(unique(x))) <= 3)) {
    message("Warning message: If a length of string is too low, it may work incorrectly")
  }
  message("Calculating..")
  acc_matrix <- adist(na.omit(unique(x)), ignore.case = ignore.case)
  rownames(acc_matrix) <- na.omit(unique(x))
  colnames(acc_matrix) <- na.omit(unique(x))
  acc_dat <- as.data.frame(acc_matrix)
  acc_dat$nchar <- nchar(rownames(acc_dat))
  result <- list()
  for(i in 1:nrow(acc_dat)){
    result[[i]] <- rownames(acc_dat[acc_dat[[i]] <= round(acc_dat$nchar *
                                                            FR), ])
  }
  result <- unique(result[unlist(lapply(result, function(x) ifelse(length(x) > 1, T, F)))])
  if(length(result) == 0)
    return(message("No problems found on this variable."))
  plotdat <- data.frame(table(x[x %in% unlist(result)]), stringsAsFactors = F)
  plotdat$fill <- NA
  for(i in 1:length(result)){
    plotdat$fill[plotdat$Var1 %in% result[[i]]] <- i
  }
  g <- ggplot2::ggplot(plotdat) +
    ggplot2::geom_bar(ggplot2::aes(x = reorder(Var1, plotdat$fill),
                                   fill = factor(fill), y = Freq), stat = "identity") +
    ggplot2::labs(x = "Values", y = "Freq") + ggplot2::guides(fill = F) + ggplot2::coord_flip()
  cat("Variable Summary")
  cat("\nCardinality :", length(na.omit(unique(x))))
  cat("\nThe Number of PBM :", length(result), "(Cases which may have syntax probelms)")
  message('\nNeed to create a object.')
  invisible(list(PBM = result, PBM.Plot = g))
}
