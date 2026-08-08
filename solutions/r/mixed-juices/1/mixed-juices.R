time_to_mix_juice <- function(juice) {
  
  switch (juice,
    "Pure Strawberry Joy" = 0.5,
    "Energizer" = 1.5,
    "Green Garden" = 1.5,
    "Tropical Island" = 3,
    "All or Nothing" = 5,
    2.5
  )
  
}

number_of_wedges <- function(lime) {
  
  switch (lime,
    small = 6,
    medium = 8,
    large = 10,
    warning("Invalid Lime")
  )
  
}

limes_to_cut <- function(needed, limes) {
  
  count <- 0
  index <- 0
  
  for (lime in seq_along(limes)) {
      if (count < needed) {
        index <- lime
        count <- count + number_of_wedges(limes[lime])
      } else {
        return (index)
      }
  }
  
  index 
}

order_times <- function(orders) {
  
  for (order in orders) {
    print(time_to_mix_juice(order))
  }
  
}

remaining_orders <- function(time_left, orders) {
  
  indices_to_remove <- c()
  
  for (order_num in seq_along(orders)) {
    if (time_left > 0) {
      time_left <- time_left - time_to_mix_juice(orders[order_num])
      indices_to_remove <- append(indices_to_remove, order_num)
    } else {
      return(orders[-indices_to_remove])
    }
  }
  
}
