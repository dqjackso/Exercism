scores_list <- function(scores) {
  scores
}

latest <- function(scores) {
  scores[length(scores)]
}

personal_best <- function(scores) {
  max(scores)
}

personal_top_three <- function(scores) {
  if (length(scores) < 3) {
    sort(scores, decreasing = TRUE)
  } else {
    sort(scores, decreasing = TRUE)[1:3]
  }
}
