success_rate <- function(speed) {
  if (speed == 0) {
    0.0
  } else if (1 <= speed && speed <= 4) {
    1.0
  } else if (5 <= speed && speed <= 8) {
    0.9
  } else if (speed == 9) {
    0.8
  } else if (speed == 10) {
    0.77
  } else {
    "INVALID SPEED"
  }
}

production_rate_per_hour <- function(speed) {
  speed * 221 * success_rate(speed)
}

working_items_per_minute <- function(speed) {
  production_rate_per_hour(speed) %/% 60
}
