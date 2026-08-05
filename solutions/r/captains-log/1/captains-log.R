library(stringr)

planetary_classes <- c("D", "H", "J", "K", "L", "M", "N", "R", "T", "Y")

random_planet_class <- function(number_needed) {

  sample(planetary_classes, size = number_needed, replace = TRUE)
  
}

random_ship_registry_number <- function() {
  
  str_glue("NCC-{sample(c(1000:9999), size = 1)}")

}

shuffle_starships <- function(starships) {
  
  sample(unique(starships))

}

random_stardate <- function() {
  
  runif(1, min = 41000.0, max = 42000.0)

}
