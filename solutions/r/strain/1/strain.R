keep <- function(input, fun) {
  if (typeof(input) == "list") {
    condition <- sapply(input, fun)
    input[condition]
  } else {
    input[fun(input)]    
  }
}

discard <- function(input, fun) {
  if (typeof(input) == "list") {
    condition <- sapply(input, fun)
    input[!condition]
  } else {
    input[!fun(input)]  
  }
}
