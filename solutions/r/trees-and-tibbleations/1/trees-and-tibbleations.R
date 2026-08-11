library(datasets)
library(tibble)
library(dplyr)

tree_data <- rename(as_tibble(trees), Diameter = Girth)

girth_n_weight <- function(data, rnd_digits) {
  
  data |>
    mutate(
      Girth = round(pi * Diameter, digits = rnd_digits),
      Weight = round(35 * Volume, digits = rnd_digits)
    )
  
}

orchard_copy <- function(data, important_cols = c("Weight", "Height")) {
  relocate(data, important_cols) |> arrange(Weight)
}

customer_copy <- function(data, min_height, max_height, max_weight) {
  filter(data, Height >= min_height, Height <= max_height, Weight <= max_weight) |>
    select(Height, Weight, Diameter, Girth)
}
