# TODO: define `colors` variable

colors <- c("black", 
            "brown", 
            "red", 
            "orange", 
            "yellow", 
            "green", 
            "blue", 
            "violet",
            "grey",
            "white")

color_code <- function(color) {
  which(colors == color) - 1
}
