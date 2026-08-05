today <- function(birds_per_day) {
  
  birds_per_day[length(birds_per_day)]
  
}

increment_todays_count <- function(birds_per_day) {
  
  c(birds_per_day[1:length(birds_per_day) - 1], birds_per_day[length(birds_per_day)] + 1)
  
}

has_day_without_birds <- function(birds_per_day) {
  
  0 %in% birds_per_day
  
}

count_for_first_days <- function(birds_per_day, num_days) {
  
  sum(birds_per_day[1:num_days])
  
}

busy_days <- function(birds_per_day) {
  
  length(birds_per_day[birds_per_day >= 5])
  
}

running_total <- function(birds_per_day) {
  
  cumsum(birds_per_day)
  
}

busy_days_of_week <- function(birds_per_day, day_names) {
  
  sort_by(day_names, birds_per_day, decreasing = TRUE)
  
}
