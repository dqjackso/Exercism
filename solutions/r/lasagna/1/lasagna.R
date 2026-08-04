# TODO: define the 'expected_minutes_in_oven()' function
expected_minutes_in_oven <- function() {
  60
}

# TODO: define the 'remaining_time_in_minutes()' function
remaining_time_in_minutes <- function(actual) {
  expected_minutes_in_oven() - actual
}

# TODO: define the 'prep_time_in_minutes()' function
prep_time_in_minutes <- function(layers) {
  layers * 2
}

# TODO: define the 'elapsed_time_in_minutes()' function
elapsed_time_in_minutes <- function(layers, time) {
  prep_time_in_minutes(layers) + time
}
