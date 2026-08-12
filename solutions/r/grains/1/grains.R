square <- function(n) {
  if (n <= 0 || n > 64) {
    stop("Square must be between 1 and 64")
  }
  
  2^(n-1)
}

total <- function() {
  sum(sapply(c(1:64), square))
}
