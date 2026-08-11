library(purrr)

compare_words <- function(subject, target) {
  tolower(subject) != tolower(target) && identical(
    sort(unlist(strsplit(tolower(subject), split = ""))),
    sort(unlist(strsplit(tolower(target), split = "")))
  )
}

anagram <- function(subject, candidates) {
  
  result <- keep(candidates, ~compare_words(subject, .x))
  
  if (length(result) == 0) {
    c()
  } else {
    result
  }
  
}
