library(stringr)

# --- Helper Functions ---

turn_robot <- function(current_dir, turn_cmd) {
  dirs <- c("NORTH", "EAST", "SOUTH", "WEST")
  curr_index <- which(dirs == current_dir)
  
  if (turn_cmd == "R") {
    new_index <- ifelse(curr_index + 1 > 4, 1, curr_index + 1)
  } else { 
    # turn_cmd == "L"
    new_index <- ifelse(curr_index - 1 < 1, 4, curr_index - 1)
  }
  
  dirs[new_index]
}

advance_robot <- function(coords, current_dir) {
  if (current_dir == "NORTH") coords[2] <- coords[2] + 1
  if (current_dir == "SOUTH") coords[2] <- coords[2] - 1
  if (current_dir == "EAST")  coords[1] <- coords[1] + 1
  if (current_dir == "WEST")  coords[1] <- coords[1] - 1
  
  coords
}

new_robot <- function(coordinates, direction) {
  
  structure(
    list(
      coordinates = coordinates,
      direction = direction
    ),
    class = "robot"
  )
  
}

move <- function(a_robot, commands) {
  UseMethod("move")
}

# nolint start
move.robot <- function(a_robot, commands) {
  instructions <- str_split_1(commands, "")
  
  for (cmd in instructions) {
    if (cmd %in% c("R", "L")) {
      a_robot$direction <- turn_robot(a_robot$direction, cmd)
    } else if (cmd == "A") {
      a_robot$coordinates <- advance_robot(a_robot$coordinates, a_robot$direction)
    }
  }
  
  a_robot
}
# nolint end
