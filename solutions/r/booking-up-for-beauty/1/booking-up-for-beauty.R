library(lubridate)
library(dplyr)

schedule_appointment <- function(appointment) {
  mdy_hms(appointment)
}

has_passed <- function(appointment) {
  appointment < now()
}

is_afternoon_appointment <- function(appointment) {
  h <- hour(appointment)
  h >= 12 && h < 18
}

day_of_week <- function(appointment) {
  wday(appointment, week_start = 1)
}

reschedule <- function(appointment) {
  a <- mdy_hms(appointment)
  day <- day_of_week(a)
  if (between(day, 1, 4)) {
    a + days((5 - day))
  } else {
    a + days((12 - day))
  }
}
