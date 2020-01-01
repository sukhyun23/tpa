linear_formula <- function(x) {
  xf <- paste(x, collapse = ' + ')
  return(xf)
}

poly_formula <- function(x, poly = 2) {
  xp <- sapply(2:poly, function(p) paste('I(', x, '^', p, ')', sep = ''))
  xp <- paste(xp, collapse = ' + ')
  return(xp)
}

inter_formula <- function(x) {
  df <- expand.grid(x1 = x, x2 = x, stringsAsFactors = F)
  df <- df[df$x1 != df$x2, ]
  
  xi <- apply(df, 1, function(x) paste(x, collapse = ':'))
  xi <- paste(xi, collapse = ' + ')
  return(xi)
}

full_formula <- function(x, y, poly = 1, inter = F) {
  xf <- linear_formula(x)
  
  if (poly > 1) {
    xp <- poly_formula(x, 2)
  } else {
    xp <- NULL
  }
  
  if (inter) {
    xi <- inter_formula(x)
  } else {
    xi <- null
  }
  
  x_formula <- c(xf, xp, xi)
  x_formula <- paste(x_formula, collapse = ' + ')
  
  result <- as.formula(paste(y, '~', x_formula))
  return(result)
  
}

revision_formula <- function(x, y) {
  is.poly <- function(x) {
    grepl('^I([[:alpha:][:digit:][:punct:]]{1,})$', x)
  }
  
  is.inter <- function(x) {
    grepl('[[:alpha:][:digit:][:punct:]]{1,}\\:[[:alpha:][:digit:][:punct:]]{1,}', x)
  }
  
  xp_idx <- is.poly(x)
  xi_idx <- is.inter(x)
  xf_idx <- !xp_idx & !xi_idx
  
  xf <- x[xf_idx]
  xp <- x[xp_idx]
  xi <- x[xi_idx]
  
  xp_f <- stringr::str_sub(xp, 3, -4)
  xi_f <- unique(unlist(strsplit(xi, ':')))
  
  xf <- unique(c(xf, xp_f, xi_f))
  
  x_formula <- linear_formula(c(xf, xp, xi))
  result <- as.formula(paste(y, '~', x_formula))
  
  return(result)
}