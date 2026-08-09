library(stringr)

is_valid_command <- function(msg) {
  str_detect(msg, regex("^chatbot", ignore_case = TRUE))
}

remove_emoji <- function(msg) {
  str_replace_all(msg, regex("emoji\\d+"), "")
}

check_phone_number <- function(number) {
  
  if (str_detect(number, regex("^\\(\\+\\d{2}\\) \\d{3}-\\d{3}-\\d{3}$"))) {
    "Thanks! You can now download me to your phone."
  } else {
    as.character(str_glue("Oops, it seems like I can't reach out to {number}"))
  }
  
}

nice_to_meet_you <- function(str) {
  name <- str_replace(str, regex("^(\\w+),\\s*(\\w+)$"), "\\2 \\1")
  as.character(str_glue("Nice to meet you, {name}"))
}

get_URL <- function(msg) {
 unlist(
   str_extract_all(
     msg,
     regex("[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}")
   )
 )
}
