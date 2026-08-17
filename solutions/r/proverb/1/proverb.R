library(stringr)

recite <- function(wanted) {
  if (length(wanted) == 0) {
    return(c())
  }
  
  paired_lines <- if (length(wanted) > 1) {
    str_glue("For want of a {wanted[-length(wanted)]} the {wanted[-1]} was lost.")
  } else {
    character(0)
  }
  
  c(paired_lines, str_glue("And all for the want of a {wanted[1]}."))
}
