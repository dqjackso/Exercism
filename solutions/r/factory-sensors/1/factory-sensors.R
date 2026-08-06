library(dplyr)
library(stringr)

check_humidity_level <- function(pct_humidity) {

  if (pct_humidity <= 70) {
    message("humidity test passed")
    return(TRUE)
  } else if (pct_humidity > 70) {
    stop("humidity is TOO HIGH")
    return(FALSE)
  } else {
    stop("error in checking humidity test")
    return(FALSE)
  }
  
}

report_overheating <- function(temperature) {

  if (is.null(temperature)) {
    stop("Sensor Broken")
    return(FALSE)
  } else if (temperature > 600) {
    stop(str_glue("Overheating: {temperature} °C"))
    return(FALSE)
  } else if (between(temperature, 501, 600)) {
    warning(str_glue("Risk of overheating: {temperature} °C"))
    return(FALSE)
  } else {
    message(str_glue("temperature check passed: {temperature} °C"))
    return(TRUE)
  }
  
}

monitor_the_machine <- function(pct_humidity, temperature) {
  
  if (check_humidity_level(pct_humidity) && report_overheating(temperature)) {
    message("All OK!")
  }
  
}
