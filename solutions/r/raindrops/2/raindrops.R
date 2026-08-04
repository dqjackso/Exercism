sounds <- c(Pling = 3, Plang = 5, Plong = 7)

raindrops <- function(number) {
  ret <- paste0(names(sounds[number %% sounds == 0]), collapse = '')

  if (nchar(ret) != 0) ret else as.character(number)
}
