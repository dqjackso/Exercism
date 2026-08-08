scale <- function(point, s) {
  point * s
}

translate <- function(point, ...) {
  point + c(...)
}

transform2d <- function(dx, dy, s = 1) {
  \(...) {
    translated_point <- translate(c(...), dx, dy)
    scale(translated_point, s)
  }
}

transform3d <- function(dx, dy, dz, s = 1) {
  \(...) {
    translated_point <- translate(c(...), dx, dy, dz)
    scale(translated_point, s)
  }
}
