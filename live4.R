# You will write several bots to play tic-tac-toe. Your player functions can
# ONLY accept a board object which is a 3x3 matrix with no other attributes or
# other tomfoolery!

# Takes a board object (3x3 matrix) and returns an integer move.
tic_tac_toe <- function(board) {
  
}

# board starts as 3x3 filled with empty strings.
# valid moves are empty squares "".
board <- matrix("", nrow = 3, ncol = 3)

# let's look at some (bad) examples...

# ttt_lazy() plays in the first available square every time:
ttt_lazy <- function(board) {
  which(board == "")[[1]]
}
ttt_lazy(board)
# ttt_rand() plays in a random available square:
# see Details and Examples in ?sample
ttt_rand <- function(board) {
  open <- which(board == "")
  open[sample.int(length(open), size = 1)]
}
ttt_rand(board)
ttt_rand(board)
ttt_rand(board)

# Write a function called play_ttt(), which takes as an argument a length-two
# vector of function names called 'players'.

# here is a VERY Rudimentary example... we need to know the ending conditions
# for the game which are when someone gets three marks in a row or when the
# board is filled. We'll use a for-loop with a break condition for this.

# pseudocode
play_ttt <- function(players) {
  # set the winner to 0
  # saet up board
  # for one move for each of the available spaces
  #   get the current player
  #   get move from one player
  #   update the board
  #   if winning move
  #     set the winner to the player number
  #     break out of the loop
  # }
  # report the winner
}

# First pass
play_ttt <- function(players) {
  # set the winner to 0
  winner <- 0 
  # set up board
  board <- matrix("", nrow = 3, ncol = 3)
  # for one move for each of the available spaces
  for (move in seq_along(board)) {
    # get the current player (pid) if there are an odd number of open spaces
    # player is player 1 or X otherwise player is player 2 or O
    pid <- sum(board != "") %% 2 + 1
    # get move from one player
    play <- players[[pid]](board)
    # update the board
    board[[play]] <- pid
    # if winning move (check_win() function)
    if (check_win(board)) {
      # set the winner to the player number
      winner <- pid
      # break out of the loop
      break
    }
  }
  # report the winner
  winner
}

# last thing we need to do is write a check_win() function. This will be easier
# later when we have access to lists, but for now we can use a matrix. Let's
# construct a matrix with all of the winning conditions.

# remember matrices in R are column major so our indices are:
#  1  4  7
#  2  5  8
#  3  6  9

# We can get three in a row with the rows:
# 1, 4, 7
# 2, 5, 8
# 3, 6, 9

# the columns:
# 1, 2, 3
# 4, 5, 6
# 7, 8, 9

# and the diagonals
# 1, 5, 9
# 3, 5, 7

# so our winners matrix could be:
wins <- c(1, 4, 7,
          2, 5, 8,
          3, 6, 9,
          1, 2, 3,
          4, 5, 6,
          7, 8, 9,
          1, 5, 9,
          3, 5, 7)
winners <- matrix(wins, nrow = 3)
winners
# then the columns of winners are the win conditions... if any one player has
# all three of any column, they win... note, we only need to check if the
# CURRENT player is the winner. Why?
check_win <- function(board, pid) {
  wins <- c(1, 4, 7,
            2, 5, 8,
            3, 6, 9,
            1, 2, 3,
            4, 5, 6,
            7, 8, 9,
            1, 5, 9,
            3, 5, 7)
  winners <- matrix(wins, nrow = 3)
  check_one <- function(win, board, pid) {
    all(board[win] == pid)
  }
  any(apply(winners, 2, check_one, board, pid))
}

# Now, our play_ttt() function looks like this:
play_ttt <- function(players, board = matrix("", nrow = 3, ncol = 3), verbose = FALSE) {
  check_win <- function(board, pid) {
    wins <- c(1, 4, 7,
              2, 5, 8,
              3, 6, 9,
              1, 2, 3,
              4, 5, 6,
              7, 8, 9,
              1, 5, 9,
              3, 5, 7)
    winners <- matrix(wins, nrow = 3)
    check_one <- function(win, board, pid) {
      all(board[win] == pid)
    }
    any(apply(winners, 2, check_one, board, pid))
  }
  winner <- 0
  move <- 1
  board <- matrix("", nrow = 3, ncol = 3)
  for (move in seq_along(board[board == ""])) {
    pid <- sum(board != "") %% 2 + 1
    play <- players[[pid]](board)
    board[[play]] <- pid
    if (check_win(board, pid)) {
      winner <- pid
      break
    }
  }
  winner
}

# simulate 1,000 games between two random players
# what proportion of the time does X win?
# what proportion of the time does O win?
n <- 1e3
res <- integer(n)
for (i in seq_along(res)) {
  res[[i]] <- play_ttt(c(ttt_rand, ttt_rand))
}
table(res)

# simulate 1,000 games between a random player playing X and a lazy player
# playing O then reverse it.
# what proportion of the time does the lazy player win as X? As O? Combined?

res2 <- integer(n)
for (i in seq_along(res2)) {
  res2[[i]] <- play_ttt(c(ttt_rand, ttt_lazy))
}
result2 <- c("tie", "rand", "lazy")[res2 + 1]
table(result2)

res3 <- integer(n)
for (i in seq_along(res3)) {
  res3[[i]] <- play_ttt(c(ttt_lazy, ttt_rand))
}
result3 <- c("tie", "lazy", "rand")[res3 + 1]
table(result3)

results_combo <- c(result2, result3)
table(results_combo) / length(results_combo)

# wow a lazy player beats a random player almost 80% of the time as X and almost
# 50% of the time as O, and over 60% of the time total!

# Now, write your own player called ttt_best(). Can you write a bot to play
# optimal tic-tac-toe vs a random opponent?

# some hints:
# 1) best vs rand, best should win ~99% of the time and never lose.
# 2) rand vs best, best should win ~91% of the time and never lose.
# 3) best vs best should always draw.

# center

