does_stack_include_card <- function(stack, card) {
  
  card %in% stack
  
}

get_card_position <- function(stack, card) {
  
  if (any(stack == card)) {
    
    return(which(stack == card))
    
  } else {
    
    return(-1)
    
  }
}

is_each_card_even <- function(stack) {
  
  all(stack %% 2 == 0)
  
}

does_stack_include_odd_card <- function(stack) {
  
  any(stack %% 2 > 0)
  
}

get_first_odd_card <- function(stack) {
  
  if (does_stack_include_odd_card(stack)) {
    
    index <- which(stack %% 2 > 0)
    
    return(stack[index[1]])
    
  } else {
    
    return(-1)
    
  }
}

get_first_even_card_position <- function(stack) {
  
  if (any(stack %% 2 == 0)) {
    
    index <- which(stack %% 2 == 0)
    
    return(index[1])
    
  } else {
    
    return(-1)
    
  }
}
