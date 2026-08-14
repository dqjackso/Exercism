library(stringr)

egg_count <- function(display_value) {

  display_value |>
    intToBits() |>
    as.character() |>
    str_count("1") |>
    sum()
  
}
