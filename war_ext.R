# Plan:
# Extensions:
# Change the deck
#   number of values in the deck -- done
#   number of suits -- done
#   add a "Joker" who always wins except against the clown -- done
#   add a "Clown" who always loses except against the joker -- done
#
# change the number of cards involved in a war (1, 2, 3, etc.) -- done
# 
# have different starting conditions
#   start one player with all the aces (13's) -- done
#
# add players -- done
#
# track certain things
#   number of battles -- done
#   number of wars -- done
#   track the number of times the joker and clown change players -- done



# init_game() notes:
# made changes so we can deal cards to any number of
# players with any combination of starting hands.
# add war_size for variable game type.
# add players to output so we don't need to continue to compute it
# add full_deck to output so we don't need to compute (change to ncards?)
# add tracking variables to compute stats
init_game <- function(players = 2,
                      decks = 1,
                      suits = 4,
                      values = 13,
                      joker = FALSE,
                      clown = FALSE,
                      initial_hands = lapply(integer(players), numeric),
                      war_size = 1,
                      ...) {
  if (length(initial_hands) > players) {
    warning("more initial hands than players, ignoring later hands")
    initial_hands <- initial_hands[seq_len(players)]
  }
  if (length(initial_hands) < players) {
    warning("some hands receive no initial cards")
    initial_hands[seq_along(initial_hands) > players] <- list(numeric(0))
  }
  full_deck <- c(rep(seq_len(values), each = decks * suits),
                 rep(Inf, joker),
                 rep(-Inf, clown))
  ffd <- factor(full_deck)
  to_remove <- unlist(initial_hands)
  ftr <- factor(to_remove, levels = levels(ffd))
  n_to_deal <- table(ffd) - table(ftr)
  if (any(n_to_deal < 0)) {
    stop("'initial_hands' have more of value than exist in the deck(s)")
  }
  player_cards <- table((seq_along(full_deck) - 1) %% players) -
    vapply(initial_hands, length, integer(1))
  deal_deck <- rep(as.numeric(levels(ffd)),  n_to_deal)
  player <- rep(names(player_cards), as.integer(player_cards))
  hand <- tapply(deal_deck, sample(player), identity)
  for (i in seq_along(hand)) {
    hand[[i]] <- sample(c(hand[[i]], initial_hands[[i]]))
  }
  init_joker <- max(0, which(vapply(hand, function(h) any(h == Inf), logical(1), USE.NAMES = FALSE)))
  list(hand = hand,
       pile = lapply(integer(players), numeric),
       pot = lapply(integer(players), numeric),
       players = seq_len(players),
       full_deck = full_deck,
       n_cards = length(full_deck),
       war_size = war_size,
       battles = 0,
       wars = 0,
       strengths = numeric(0),
       init_joker = init_joker,
       joker = 0,
       clown = 0)
}

get_ncards <- function(p, game) {
  with(game,
       length(hand[[p]]) +
         length(pile[[p]]) + 
         length(unlist(pot)))
}

# is_endgame notes:
# use n_cards object from game object
is_endgame <- function(game) {
  with(game, {
    ncards <- vapply(seq_along(hand), get_ncards, integer(1), game)
    any(ncards == n_cards)
  })
}

# refresh_hands notes:
# deprecated in favor of refresh_hand()
# only refresh a hand if needed
# move check for this from after playing a card to before playing any number of
# cards

# refresh_hands <- function(game) {
#   for (i in seq_along(game[["hand"]])) {
#     if (length(game[["hand"]][[i]]) == 0) {
#       game[["hand"]][[i]] <- sample(game[["pile"]][[i]])
#       game[["pile"]][[i]] <- numeric(0)
#     } 
#   }
#   game
# }

refresh_hand <- function(p, game) {
  c(game[["hand"]][[p]], sample(game[["pile"]][[p]]))
}

# resolve_battle() notes:
# remove NA values from pile before putting into winner's pile
# added code to identify when joker and clown change players
resolve_battle <- function(game, winner) {
  with(game, {
    pile <- pile[[winner]]
    if (any(which(vapply(pot,
                         function(p) {
                           any(p == Inf)
                         },
                         logical(1))) != winner)) {
      game[["joker"]] <- game[["joker"]] + 1
    }
    if (any(which(vapply(pot,
                         function(p) {
                           any(p == -Inf)
                         },
                         logical(1))) != winner)) {
      game[["clown"]] <- game[["clown"]] + 1
    }
    pile <- c(pile, unlist(pot))
    pile <- pile[!is.na(pile)]
    game[["pile"]][[winner]] <- pile
    for(i in seq_along(game[["hand"]])) {
      game[["pot"]][[i]] <- numeric(0)
    }
    game  
  })
}

# play_cards() notes:
# allow playing cards from a subset of players
# allow playing more than one card at a time to allow for clean wars
# allow playing NA cards when the player cannot play as many cards as required
play_cards <- function(game,
                       players = seq_along(game[["hand"]]),
                       n = 1) {
  for (p in players) {
    hand <- game[["hand"]][[p]]
    if (length(hand) < n) {
      hand <- refresh_hand(p, game)
      game[["hand"]][[p]] <- hand
      game[["pile"]][[p]] <- numeric(0)
    }
    idx <- seq_len(n)
    game[["pot"]][[p]] <- c(game[["pot"]][[p]], game[["hand"]][[p]][idx])
    game[["hand"]][[p]] <- game[["hand"]][[p]][-idx]
  }
  game
}

# get_winner notes:
# no pot should never be length-zero as we allow selection of NA's now.
# add ability to check for clown/joker clash
# added cyclic check for clown/joker
#
# Question:
# what to do in case of three way split? e.g. clown < 8 < joker < clown
# in this case: clown wins once loses once (1:1),
#               8 goes 1:1
#               joker goes 1:1
# instinct says tie.
# what about 4 way e.g. clown < 4 < 8 < joker < clown
# counting:     clown:  1:2
#               4:      1:2
#               8:      2:1
#               joker:  2:1
# tie between 8 and joker? that doesn't make sense
# simplest choice:
# declare a tie if there is both joker and clown with any other cards
get_winner <- function(pots) {
  battle_card <- vapply(pots,
                        function(pot) {
                          max(tail(pot, 1))
                        },
                        numeric(1))
  out <- is.na(battle_card)
  if (any(battle_card == -Inf, na.rm = TRUE) &
      any(battle_card == Inf, na.rm = TRUE)) {
    if (any(abs(battle_card) < Inf, na.rm = TRUE)) {
      winner <- which(!out)
    } else {
      winner <- which(battle_card == -Inf)
    }
  } else {
    winner <- which(battle_card == max(battle_card, na.rm = TRUE) & !out)
  }
  winner
}

# do_battle notes:
# change to while loop so we resolve the entirety of a battle in one go
# only winners continue playing (only an issue for players > 2)
# war_size can vary by player now!
# added code to track the numbers of battles and wars
do_battle <- function(game) {
  game[["battles"]] <- game[["battles"]] + 1
  game[["strengths"]] <- cbind(game[["strengths"]], get_strengths(game))
  game <- play_cards(game)
  winner <- get_winner(game[["pot"]])
  while (length(winner) > 1) {
    game[["wars"]] <- game[["wars"]] + 1
    game <- play_cards(game, winner, n = 1 + game[["war_size"]])
    winner <- get_winner(game[["pot"]])
  }
  resolve_battle(game, winner)
}

get_strengths <- function(game) {
  with(game, {
    jok_val <- max(full_deck[!is.infinite(full_deck)]) + 1
    vapply(players,
           function(p) {
             cards <- c(hand[[p]], pile[[p]])
             max(sqrt(mean(pmax(pmin(cards, jok_val), 0)^2)), 0, na.rm = TRUE)
           },
           numeric(1))
  })
}

# endgame() notes:
# change the return type to be a list object with more information
endgame <- function(game) {
  with(game, {
    hand <- lapply(players, refresh_hand, game)
    winner <- which(vapply(hand, length, integer(1)) > 0)
    list(winner = winner,
         battle = battles,
         wars = wars,
         strengths = list(t(strengths)),
         init_joker = init_joker,
         joker = joker,
         clown = clown)
  })
}

play_war <- function(...) {
  verbose <- list(...)[["verbose"]]
  game <- init_game(...)
  players <- game[["players"]]
  nplayers <- length(players) + 1
  while(!is_endgame(game)) {
    if (!is.null(verbose)) {
      if (verbose >= 2) {
        strengths <- get_strengths(game)
        if (sum(strengths > 0) < nplayers) {
          nplayers <- sum(strengths > 0)
          message(nplayers, " players remaining")
          message(game[["battles"]], " battles fought")
        }  
      } else if (verbose >= 1) {
        
      }
    }
    game <- do_battle(game)
  }
  endgame(game)
}

prep_results <- function(n, ...) {
  args <- list(...)
  results <- data.frame(winner = integer(n),
                        battles = integer(n),
                        wars = integer(n))
  strengths <- lapply(seq_len(n), function(i) list())
  results[["strengths"]] <- strengths
  results[["init_joker"]] <- integer(n)
  if (!is.null(args[["joker"]]) && args[["joker"]] > 0) {
    results[["joker"]] <- integer(n)
  }
  if (!is.null(args[["clown"]]) && args[["clown"]] > 0) {
    results[["clown"]] <- integer(n)
  }
  results
}

do_sim <- function(n, ...) {
  verbose <- list(...)[["verbose"]]
  if (!is.null(verbose)) {
    k <- max(list(...)[["k"]], 10)
    og_start <- start <- Sys.time()
    message("Simulation started at: ", start)
  }
  results <- prep_results(n, ...)
  for (i in seq_len(n)) {
    res <- play_war(...)
    if (!is.null(verbose)) {
      if (verbose >= 1) {
        if (i %% k == 0) {
          end <- Sys.time()
          message("\fSimulation started at: ", og_start)
          message(i, "th iteration completed at: ", end)
          message("taking ", format(time <- diff.Date(c(start, end)), digits = 3), " for ", k, " runs.")
          message("expected ", format(diff.Date(c(og_start, end)) / i * (n - i), digits = 3), " remaining")
          start <- end
        }
      }
    }
    attr(res, "class") <- "data.frame"
    attr(res, "row.names") <- 1L
    attributes(res)
    results[i, ] <- res
  }
  results
}

get_init_strength_leads <- function(res) {
  strengths <- res[["strengths"]]
  init_strengths <- t(vapply(strengths, `[`, numeric(ncol(strengths[[1]])), i = 1,))
  t(apply(init_strengths,
        1,
        function(x) {
          c(max(x) - max(x[x < max(x)]), which(x == max(x))[[1]])
        }))
}

win_strength_confusion_matrix <- function(res, delta) {
  win <- res[["winner"]]
  strength_diff <- get_init_strength_leads(res)
  idx_wide <- strength_diff[, 1] >= delta
  table(lead = strength_diff[idx_wide, 2], win = win[idx_wide])
}

# if one player has a initial strength 0.5 points higher than the second
# strongest hand, what proportion of games do you expect the strongest hand to
# win?
do_sim(1e3, players = 2, joker = TRUE, clown = TRUE, verbose = TRUE)

wscm <- win_strength_confusion_matrix(res, 0.5)
sum(diag(wscm)) / sum(wscm)
isl <- get_init_strength_leads(res)
idx <- isl[, 1] >= 2.5

# how many battles do we expect, on average, when one player has an initial hand
# strength larger than the second highest player? by increments of 0.5
breaks <- cut(isl[, 1], breaks = seq(0, max(isl[, 1]) + 0.5, by = 0.5))
tapply(res[["battles"]], breaks, mean)
# how many are in each increment?
tapply(res[["battles"]], breaks, length)

# what are the quantiles for number of battles in each increment?
tapply(res[["battles"]], breaks, quantile)

# simulate 1000 games between 5 players with 4 standard decks plus the joker and
# clown. Make sure it's working correctly before you run it, it can take upwards
# of an hour to complete on a modern PC. If you're not able to run it overnight,
# try doing fewer iterations, as many as you can do in about an hour (500, 200,
# or 100).
# Then answer the following questions:

# How often does the player who starts with the joker win?
res <- do_sim(1e3, players = 5, decks = 4, joker = TRUE, clown = TRUE, verbose = 1)
win <- res[["winner"]]
joker_start <- res[["init_joker"]]
mean(win == joker_start)
# I get ~ 37%

# how often does the joker never get traded?
mean(res[["joker"]] == 0)


# on average, how many battles were for before the first player was eliminated?
get_knockout <- function(strength, nko = 1) {
  nplayers <- apply(strength, 1, function(r) sum(r > 0))
  min(which(nplayers <= ncol(strength) - nko))  
}
knockout1 <- vapply(res[["strengths"]], get_knockout, integer(1))
last_knockout1 <- min(which(knockout1 == max(knockout1)))
lkob <- res[["battles"]][[last_knockout1]]
max(res[["battles"]])
mean(res[["battles"]] > lkob)
# is there any correlation between when the first knockout happens and how long
# the war lasts?
cor(knockout1, res[["battles"]])

# is there any correlation between when the third knockout happens and how long
# the war lasts?
knockout3 <- vapply(res[["strengths"]], get_knockout, integer(1), nko = 3)
hist(knockout3)
last_knockout3 <- min(which(knockout3 == max(knockout3)))
lkob <- res[["battles"]][[last_knockout3]]
max(res[["battles"]])
mean(res[["battles"]] > lkob)

cor(knockout3, res[["battles"]])

# simulate 10,000 games between 2 players with 1 standard deck. The
# first time let the war size be the default 1, the second make the war size 4.
# (this should take about 10 and 4 minutes each to run.)
# Then answer the following questions:

# Compare the number of battles required to complete a game.
res1 <- do_sim(1e4, players = 2, verbose = TRUE, k = 100)
res2 <- do_sim(1e4, players = 2, war_size = 4, verbose = TRUE, k = 100)
b1 <- res1[["battles"]]
b2 <- res2[["battles"]]
summary(b1)
summary(b2)