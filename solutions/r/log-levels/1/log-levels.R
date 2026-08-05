library(stringr)

log_split <- function(msg) {
  # Splits a log line into the level and message components separately
  # Returns character vector of components
  
  split <- str_split_1(msg, "]: ")
  
  c(str_sub(split[1], start = 2), split[2])
  
}

message <- function(msg) {
  # Return a log lines message
  # Remove any leading/trailing white space

  str_squish(log_split(msg)[2])
  
}

log_level <- function(msg) {
  # Return the level of the log line, in lowercase

  str_to_lower(log_split(msg)[1])
  
}

reformat <- function(msg) {
  # Message first, log line after in parenthesis, lowercase
  
  str_glue("{message(msg)} ({log_level(msg)})")
  
}
