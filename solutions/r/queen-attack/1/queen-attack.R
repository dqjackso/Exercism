library(dplyr)

create <- function(row, col) {
  if (all(between(c(row, col), 0, 7))) {
    c(row,col)
  } else {
    stop("Rows and Columns must be 0 <= row/col <= 7")
  }
}

can_attack <- function(queen1, queen2) {
  if (identical(queen1, queen2)) {
    stop("Queens cannot be on the same square")
  }
  
  queen1[1] == queen2[1] || 
    queen1[2] == queen2[2] || 
    abs(queen1[1] - queen2[1]) == abs(queen1[2] - queen2[2])
}
