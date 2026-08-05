library(stringr)

no_ID_name_badge <- function(name, department) {
  
  str_glue("{name} - {str_to_upper(department)}")

}

with_ID_name_badge <- function(id, name, department) {
  
  str_glue("[{id}] - {name} - {str_to_upper(department)}")
  
}

decide_ID_or_not <- function(id, name, department) {
  
  if (is.na(id)) {
    no_ID_name_badge(name, department)
    
  } else {

    with_ID_name_badge(id, name, department)
    
  }
  
}

print_name_badge <- function(id, name, department) {
  
  if (is.null(department)) {
    
    decide_ID_or_not(id, name, "owner")
    
  } else {
    
    decide_ID_or_not(id, name, department)
    
  }
  
}

salaries_no_id <- function(ids, salaries) {
  
  sum(salaries[which(is.na(ids))])
  
}
