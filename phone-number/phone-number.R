library(stringr)

parse_phone_number <- function(number_string) {
  
  cleaned <- clean_phone_number(number_string)
  last_ten <- get_last_ten(cleaned)

  if (str_detect(number_string, regex("[a-zA-Z]"))) {
    NULL
  } else if (nchar(cleaned) < 10) {
    NULL
  } else if (nchar(cleaned) > 11) {
    NULL
  } else if (nchar(cleaned) == 11 && substr(cleaned, 0, 1) != 1) {
    NULL
  } else if (substr(last_ten, 1, 1) < 2 || substr(last_ten, 4, 4) < 2) {
    NULL
  } else {
    last_ten
  }

}

check_phone_number_length <- function(clean_number_string) {
  # Check if phone number is at least 10 digits but no more than 11 digits
  if (nchar(clean_number_string) < 10) {
    NULL
  } else if (nchar(clean_number_string) > 11) {
    NULL
  }
}

check_country_code <- function(clean_number_string) {
  # Check if the country code is not 1 if the phone number is 11 digits long
  if ((nchar(clean_number_string) == 11) && (substr(clean_number_string, 0, 1) != 1)) {
    NULL
  }
}

check_for_correct_digits <- function(clean_number_string) {
  # Check for the 0th and 3rd digit to be < 2
  if ((substr(clean_number_string, 0, 1) < 2) || (substr(clean_number_string, 3, 4) < 2)) {
    NULL
  }
}

get_last_ten <- function(clean_number_string) {
  # Return only the last 10 digits for correct NANP format
  substring(clean_number_string, nchar(clean_number_string) - 10 + 1)
}

check_for_letters <- function(number_string) {
  # Check for letters
  if (str_detect(number_string, regex("[a-zA-Z]"))) {
    NULL
  }
}

clean_phone_number <- function(number_string) {
  # Function that takes string and returns the string without spaces or punctuation
  new <- remove_punctuation(number_string)
  remove_spaces(new)
}

remove_spaces <- function(number_string) {
  # Removes spaces from string
  str_replace_all(number_string, regex("\\s*"), "")
}

remove_punctuation <- function(number_string) {
  # Removes punctuation from string
  str_replace_all(number_string, regex("[^\\w\\s]+"), "")
}