new_car <- function(speed, battery_drain) {
  
  list(
    speed = speed,
    battery_drain = battery_drain,
    battery = 100,
    distance_traveled = 0
  )
  
}

new_track <- function(track_length) {
  
  list(
    track_length = track_length
  )
  
}

battery_drained <- function(car) {
  
  car$battery < car$battery_drain
  
}

drive <- function(car) {
  
  if (battery_drained(car)) {
    return(car)
  } else {
    car$distance_traveled = car$distance_traveled + car$speed
    car$battery = car$battery - car$battery_drain
    return(car)
  }
  
}

can_finish <- function(car, track) {
  
  battery_needed <- ceiling((track$track_length / car$speed)) * car$battery_drain
  
  car$battery >= battery_needed
  
}

store_track <- function(car, track, name) {
  
  car$battery = 100
  car$distance_traveled = 0
  
  track$complete <- can_finish(car, track)
  
  car[[paste0(name)]] = track
  
  car
  
}
