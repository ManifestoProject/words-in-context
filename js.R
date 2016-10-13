# Kullback-Leibler Divergence

kld <- function(x,y) {
  if (all(is.na(x)) && all(is.na(y))) {
    return(NA)
  } else {
    sum(x*log2(x/y), na.rm = TRUE)      
  }
}

sqd <- function(x, y) {
  sqrt(sum((ifelse(is.na(x), 0.0, x) - ifelse(is.na(y), 0.0, y))^2))
}

smooth_kld <- function(x, y, lambda = 1) {
  kld( (x + lambda/length(x))/(1+lambda), (y + lambda/length(y))/(1+lambda) )
}


# Jensen-Shannon Divergence

js <- function(x,y) {
  m <- (x+y)/2
  0.5*(kld(x,m)+kld(y,m))
}

multi_js <- function(l, fun = kld, ...) {
  
  l <- subset(l, !unlist(lapply(l, . %>% is.na() %>% all())))
  
  m <- l %>%
    as.data.frame() %>%
    rowSums(na.rm = TRUE) %>%
    divide_by(length(l))
  l %>%
    lapply(fun, m, ...) %>%
    unlist() %>%
    sum() %>%
    divide_by(length(l))
}

