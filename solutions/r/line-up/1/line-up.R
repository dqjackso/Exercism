library(stringr)
library(scales)

# - Numbers ending in 1 (unless ending in 11) → `"st"`
# - Numbers ending in 2 (unless ending in 12) → `"nd"`
# - Numbers ending in 3 (unless ending in 13) → `"rd"`
# - All other numbers → `"th"`
# 
# "Mary, you are the 1st customer we serve today. Thank you!"

line_up <- function(name, number) {
  
  str_glue("{name}, you are the {label_ordinal()(number)} customer we serve today. Thank you!")
  
}
