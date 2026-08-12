library(stringr)

letter_replace <- function(letter) {
  switch (letter,
    "G" = "C",
    "C" = "G",
    "T" = "A",
    "A" = "U",
    stop("Invalid DNA letter")
  )
}

to_rna <- function(dna) {
  dna |>
    str_split(pattern = "") |>
    unlist() |>
    sapply(letter_replace) |>
    str_c(collapse = "")
}
