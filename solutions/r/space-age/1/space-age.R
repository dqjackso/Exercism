space_age <- function(seconds, planet) {

  # find correct planet
  switch (planet,
    "mercury" = multiplier <- 0.2408467,
    "venus" = multiplier <- 0.61519726,
    "earth" = multiplier <- 1,
    "mars" = multiplier <- 1.8808158,
    "jupiter" = multiplier <- 11.862615,
    "saturn" = multiplier <- 29.447498,
    "uranus" = multiplier <- 84.016846,
    "neptune" = multiplier <- 164.79132
  )
  # find planet seconds for 1 year
  seconds_in_year <- multiplier*365.25*24*60*60
  
  # divide seconds by planet seconds
  round(seconds / seconds_in_year, digits = 2)
}
