square_root <- function(number) {
  if (number == 0) return(0)
  if (number < 0) stop("Cannot compute square root of negative number.")
  if (number == 1) return(1)
  
  guess <- number / 2
  tolerance <- 1e-10
  
  while(abs(guess^2 - number) > tolerance) {
    guess <- 0.5 * (guess + number / guess)
  }
  
  guess
}
