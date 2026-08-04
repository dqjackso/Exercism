raindrops <- function(number) {
  ret = ""
  
  if (!number %% 3) {
    ret = paste(ret, "Pling", sep = "")
  } 
  
  if (!number %% 5) {
    ret = paste(ret, "Plang", sep = "")
  } 
  
  if (!number %% 7) {
    ret = paste(ret, "Plong", sep = "")
  } 
  
  if (!nchar(ret)) {
    sprintf("%s", number)
  } else {
    print(ret)
  }
}
