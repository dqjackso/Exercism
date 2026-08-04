# This is a stub function to take two strings
# and calculate the hamming distance

hamming <- function(strand1, strand2) {
  if (nchar(strand2) != nchar(strand1)) {
    stop()
  } else if (nchar(strand1) == 0 || nchar(strand2) == 0) {
    0
  } else {
    first <- strsplit(strand1, split = "")
    
    second <- strsplit(strand2, split = "")
    
    count <- 0
    
    for (i in 1:nchar(strand1)) {
      if (first[[1]][i] != second[[1]][i]) {
        count <- count + 1
      }
    }
    
    count
  }
}
