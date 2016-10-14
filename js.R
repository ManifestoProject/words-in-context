# Kullback-Leibler Divergence

kld <- function(x,y) {
  if (all(is.na(x)) && all(is.na(y))) {
    return(NA)
  } else {
    sum(x*log2(x/y), na.rm = TRUE)      
  }
}

entropy <- function(x) {
  if (all(is.na(x))) {
    return(NA)
  } else {
    -sum(x*log2(x), na.rm = TRUE)      
  }
}

max_entropy <- function(n) {
  -n*(1/n * log2(1/n))
}

rel_entropy <- function(x) {
  entropy(x)/max_entropy(length(x))
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

multi_js <- function(l, lambda = 10, ...) {
  
  l <- l %>%
    subset(unlist(lapply(l, . %>% is.na() %>% not() %>% sum())) > 1) %>%
    lapply(. %>%
             { ifelse(is.na(.), lambda/length(.), . + lambda/length(.)) } %>%
             divide_by(1 + lambda/length(.)))

  h <- l %>%
    as.data.frame() %>%
    rowSums(na.rm = TRUE) %>%
    divide_by(length(l)) %>%
    rel_entropy()
  
  l %>%
    lapply(rel_entropy) %>%
    unlist() %>%
    sum() %>%
    divide_by(length(l)) %>%
    { h - . }
  
}


