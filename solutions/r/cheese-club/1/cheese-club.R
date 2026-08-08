library(purrr)

all_15 <- function(ratings) {
  every(ratings, \(rating) rating %in% c(1,5))
}

name_customers <- function(names, ratings) {
  map2(names, ratings, ~list(name = .x, rating = .y))
}

emphatics <- function(names, ratings) {
  customers <- name_customers(names, ratings)
  keep(customers, ~ all_15(.x$rating))
}

to_binary <- function(ratings) {
  map_int(ratings, ~ as.numeric(.x == 5))
}

satisfactions <- function(ratings) {
  averages <- cumsum(ratings) / seq_along(ratings)
  map_dbl(averages, ~ round(.x, 2))
}

