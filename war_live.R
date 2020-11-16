init_game <- function(cards = rep(seq_len(13), 4)) {
  player <- seq_along(cards) %% 2
  hand <- tapply(sample(cards), player, identity)
  list(hand = hand,
       pile = list(integer(0), integer(0)),
       pot = list(integer(0), integer(0)))
}

get_ncards <- function(p, game_state) {
  length(game_state[["hand"]][[p]]) +
    length(game_state[["pile"]][[p]]) + 
    length(unlist(game_state[["pot"]]))
}

is_endgame <- function(game_state) {
  idx <- seq_along(game_state[["hand"]])
  ncards <- vapply(idx, get_ncards, integer(1), game_state)
  any(ncards == length(unlist(game_state)))
}

refresh_hands <- function(game_state) {
  for (i in seq_along(game_state[["hand"]])) {
    if (length(game_state[["hand"]][[i]]) == 0) {
      game_state[["hand"]][[i]] <- sample(game_state[["pile"]][[i]])
      game_state[["pile"]][[i]] <- integer(0)
    } 
  }
  game_state
}

resolve_battle <- function(game_state, winner) {
  pile <- game_state[["pile"]][[winner]]
  pile <- c(pile, unlist(game_state[["pot"]]))
  game_state[["pile"]][[winner]] <- pile
  for(i in seq_along(game_state[["hand"]])) {
    game_state[["pot"]][[i]] <- integer(0)
  }
  game_state
}

do_battle <- function(game_state) {
  play_cards <- function(game_state) {
    for(i in seq_along(game_state[["hand"]])) {
      if (length(game_state[["hand"]][[i]]) > 0) {
        game_state[["pot"]][[i]] <- c(game_state[["pot"]][[i]],
                                      game_state[["hand"]][[i]][[1]])
        game_state[["hand"]][[i]] <- game_state[["hand"]][[i]][-1]
      }
    }
    refresh_hands(game_state)
  }
  get_winner <- function(game_state) {
    battle_card <- vapply(game_state[["pot"]],
                          function(pot) {
                            max(tail(pot, 1), -Inf)
                          },
                          numeric(1))
    which(battle_card == max(battle_card))
  }

  game_state <- play_cards(game_state)
  if (length(winner) > 1) {
    for(i in seq_along(game_state[["hand"]])) {
      if (length(game_state[["hand"]][[i]]) != 0) {
        game_state <- play_cards(game_state, i)
      } else {
        winner <- winner[-i]
      }
    }
  }
  if (length(winner) == 1) {
    game_state <- resolve_battle(game_state, winner)
  }
  game_state
}

endgame <- function(game_state) {
  idx <- seq_along(game_state[["hand"]])
  ncards <- vapply(idx, get_ncards, integer(1), game_state)
  which(ncards == length(unlist(game_state)))
}

play_war <- function() {
  game_state <- init_game()
  while(!is_endgame(game_state)) {
    game_state <- do_battle(game_state)
  }
  endgame(game_state)
}
play_war()

result <- integer(100)
for (i in seq_along(result)) {
  if (i %% 100 == 0) {
    message(i, "th iteration")
  }
  result[[i]] <- play_war()
}
table(result)

# Extensions:
# Change the deck
#   number of values in the deck
#   number of suits
#   add a "Joker" who always wins except against the clown
#   add a "Clown" who always loses except against the joker
#
# change the number of cards involved in a war (1, 2, 3, etc.)
# 
# have different starting conditions
#   start one player with all the aces (13's)
#
# add players
#
# track certain things
#   number of battles
#   number of wars
#   track the number of times the joker and clown change players
