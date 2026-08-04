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

value <- function(resistor_colors) {
  as.integer(
    paste0(
      which(colors == resistor_colors[1]) - 1,
      which(colors == resistor_colors[2]) - 1
    )
  )
}
