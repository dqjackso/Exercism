library(stringr)

acronym <- function(input) {
  input |>
    str_replace_all("['’][sS]\\b", "") |>
    str_split(pattern = "[[:punct:][:space:]]+") |>
    unlist() |>
    str_split_i(pattern = "", 1) |>
    str_to_upper() |>
    str_c(collapse = "")
}
