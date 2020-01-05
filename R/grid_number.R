grid_number <- function(n, N) {
  start <- seq(1, N, by = n)
  end <- seq(1, N, by = n) + (n-1)
  
  if (any(end > N)) {
    numb <- length(which(end > N))
    end <- end[-which(end > N)]
    end <- c(end, rep(N, numb))
  }
  
  idx_list <- list()
  for (i in 1:length(start)) {
    idx_list[[i]] <- seq(start[i], end[i], by = 1)
  }
  
  return(idx_list)
}