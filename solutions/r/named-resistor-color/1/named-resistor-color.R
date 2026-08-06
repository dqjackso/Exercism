resistor_bands <- c(
  "black" = 0,
  "brown" = 1,
  "red" = 2,
  "orange" = 3,
  "yellow" = 4,
  "green" = 5,
  "blue" = 6,
  "violet" = 7,
  "grey" = 8,
  "white" = 9
)

band_value <- function(band) {
  
  unname(resistor_bands[band])
  
}

two_band_value <- function(bands) {
  
  value <- unname(resistor_bands[bands[1]]) * 10 + unname(resistor_bands[bands[2]])
  
}

ohms <- function(bands) {
  
  two_band_value(bands) * 10 ^ band_value(bands[3])
  
}
