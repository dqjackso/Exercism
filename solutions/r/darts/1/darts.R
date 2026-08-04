score <- function(x, y) {
  dart_radius = sqrt((x^2 + y^2))
  if (dart_radius <= 1) {
    10
  } else if (dart_radius <= 5) {
    5
  } else if (dart_radius <= 10) {
    1
  } else {
    0
  }
}
