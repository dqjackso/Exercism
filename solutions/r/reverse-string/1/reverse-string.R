library(stringr)

# Uncomment the line below to enable grapheme cluster tests

enable_grapheme_clusters <- TRUE

reverse <- function(text) {
    text |>
    str_split(boundary("character")) |>
    unlist() |>
    rev() |>
    paste(collapse = "")
}
