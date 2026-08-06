library(dplyr)

parse_card <- function(card) {
  
  switch(card,
         ace = 11,
         two = 2,
         three = 3,
         four = 4,
         five = 5,
         six = 6,
         seven = 7,
         eight = 8,
         nine = 9,
         ten = 10,
         jack = 10,
         queen = 10,
         king = 10,
         0)
  
}

first_turn <- function(card1, card2, dealer_card) {
  
  card1_value <- parse_card(card1)
  card2_value <- parse_card(card2)
  dealer_card_value <- parse_card(dealer_card)
  
  my_card_value <- sum(card1_value, card2_value)
  
  case_when(
    my_card_value == 22 ~ "P",
    my_card_value == 21 && dealer_card_value <= 9 ~ "W",
    my_card_value == 21 && dealer_card_value > 9 ~ "S",
    between(my_card_value, 17, 20) ~ "S",
    between(my_card_value, 12, 16) && dealer_card_value >= 7 ~ "H",
    between(my_card_value, 12, 16) ~ "S",
    my_card_value <= 11 ~ "H",
    .default = "error in card values"
  )
  
}
