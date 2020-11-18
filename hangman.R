# Hangman guesser

# need: dictionary of words
# https://boardgames.stackexchange.com/questions/38366/latest-collins-scrabble-words-list-in-text-file
set.seed(147)
words <- tolower(read.delim("words.txt")[[1]])
length(words)
word <- sample(words[nchar(words) == 12], 1)
make_game <- function(word) {
  list(word = word,
       guessed = character(0),
       missed = 0,
       pattern = character(nchar(word)))
}

make_neg_pat <- function(guessed) {
  if (length(guessed) > 0) {
    paste("[^", paste(guessed, collapse = ""), "]", sep = "")
  } else {
    "\\w"
  }
}

update_game <- function(letter, game) {
  game[["guessed"]] <- c(game[["guessed"]], letter)
  if (!grepl(letter, game[["word"]])) {
    game[["missed"]] <- game[["missed"]] + 1
  }
  game[["pattern"]] <- make_placeholder(game[["word"]], game[["guessed"]])
  print(game[["pattern"]])  
  game
}


get_best_guess <- function(game, words) {
  game[["pattern"]][game[["pattern"]] == ""] <- make_neg_pat(game[["guessed"]])
  ox <- words[grepl(paste("\\b", paste(game[["pattern"]], collapse = ""),"\\b", sep = ""), words)]
  if (length(game[["guessed"]]) > 0) {
    guessed_pat <- paste("[", paste(game[["guessed"]], collapse = ""), "]", sep = "")
    x <- gsub(pattern = guessed_pat, replacement = "", ox)
  }
  message("Number of possible words remaining: ", length(ox))
  if (length(ox) < 20) {
    print(ox)
  }
  names(sort(table(unlist(lapply(strsplit(x, ""), unique))), decreasing = TRUE)[1])
}

play_game <- function(word) {
  game <- make_game(word)
  while (any(game[["pattern"]] == "") && game[["missed"]] < 10) {
    guess <- get_best_guess(game, words)
    game <- update_game(guess, game)
  }
  game
}
word <- sample(words[nchar(words) == 13], 1)
play_game(word)
