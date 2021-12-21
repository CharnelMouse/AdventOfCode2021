start_positions <- readLines("21.txt") |>
  substring(nchar("Player X starting position: ") + 1L) |>
  as.integer()

# Player 1: adds 18n-12 on turn n.
# Player 2: add 18n-3 on turn n.

mod2 <- function(x, base) {(x - 1L) %% base + 1L}

p1_pos <- start_positions[1]
p1_scores <- integer()
p1_score <- 0L
p2_pos <- start_positions[2]
p2_scores <- integer()
p2_score <- 0L
turn <- 1L
while (p1_score < 1000 && p2_score < 1000) {
  p1_pos <- mod2(p1_pos + 18L*turn - 12L, 10L)
  p1_score <- p1_score + p1_pos
  p1_scores <- c(p1_scores, p1_score)
  p2_pos <- mod2(p2_pos + 18L*turn - 3L, 10L)
  p2_score <- p2_score + p2_pos
  p2_scores <- c(p2_scores, p2_score)
  turn <- turn + 1L
}
P1 <- if (p1_scores[turn - 1L] >= 1000) {
  p2_scores[turn - 2L]*(6L*(turn - 1L) - 3L)
}else{
  p1_scores[turn - 1L]*(6L*(turn - 1L))
}
P1 # part one: 576600

# states are pos1 x score1 x pos2 x score2 freqs, so 10x31x10x31.

state <- array(0, dim = c(10, 31, 10, 31))
state[start_positions[1], 1, start_positions[2], 1] <- 1
p1_wins <- 0
p2_wins <- 0
turn <- 1L

# 3d3 has distribution 1x3, 3x4, 6x5, 7x6, 6x7, 3x8, 1x9.

while (sum(state) > 0) {
  # P1 turn
  state_after_move <-
    1*state[c(8:10, 1:7), , , ] +
    3*state[c(7:10, 1:6), , , ] +
    6*state[c(6:10, 1:5), , , ] +
    7*state[c(5:10, 1:4), , , ] +
    6*state[c(4:10, 1:3), , , ] +
    3*state[c(3:10, 1:2), , , ] +
    1*state[c(2:10, 1), , , ]
  for (p2p in 1:10) {
    for (p2s in 1:31) {
      state[ 1, , p2p, p2s] <- c(rep(0,  1), state_after_move[ 1, -(31:31), p2p, p2s])
      state[ 2, , p2p, p2s] <- c(rep(0,  2), state_after_move[ 2, -(30:31), p2p, p2s])
      state[ 3, , p2p, p2s] <- c(rep(0,  3), state_after_move[ 3, -(29:31), p2p, p2s])
      state[ 4, , p2p, p2s] <- c(rep(0,  4), state_after_move[ 4, -(28:31), p2p, p2s])
      state[ 5, , p2p, p2s] <- c(rep(0,  5), state_after_move[ 5, -(27:31), p2p, p2s])
      state[ 6, , p2p, p2s] <- c(rep(0,  6), state_after_move[ 6, -(26:31), p2p, p2s])
      state[ 7, , p2p, p2s] <- c(rep(0,  7), state_after_move[ 7, -(25:31), p2p, p2s])
      state[ 8, , p2p, p2s] <- c(rep(0,  8), state_after_move[ 8, -(24:31), p2p, p2s])
      state[ 9, , p2p, p2s] <- c(rep(0,  9), state_after_move[ 9, -(23:31), p2p, p2s])
      state[10, , p2p, p2s] <- c(rep(0, 10), state_after_move[10, -(22:31), p2p, p2s])
    }
  }
  p1_wins <- p1_wins + sum(state[, 22:31, , ])
  state[, 22:31, , ] <- 0

  # P2 turn
  state_after_move <-
    state[, , c(8:10, 1:7), ] +
    3L*state[, , c(7:10, 1:6), ] +
    6L*state[, , c(6:10, 1:5), ] +
    7L*state[, , c(5:10, 1:4), ] +
    6L*state[, , c(4:10, 1:3), ] +
    3L*state[, , c(3:10, 1:2), ] +
    state[, , c(2:10, 1), ]
  for (p1p in 1:10) {
    for (p1s in 1:31) {
      state[p1p, p1s,  1, ] <- c(rep(0,  1), state_after_move[p1p, p1s,  1, -(31:31)])
      state[p1p, p1s,  2, ] <- c(rep(0,  2), state_after_move[p1p, p1s,  2, -(30:31)])
      state[p1p, p1s,  3, ] <- c(rep(0,  3), state_after_move[p1p, p1s,  3, -(29:31)])
      state[p1p, p1s,  4, ] <- c(rep(0,  4), state_after_move[p1p, p1s,  4, -(28:31)])
      state[p1p, p1s,  5, ] <- c(rep(0,  5), state_after_move[p1p, p1s,  5, -(27:31)])
      state[p1p, p1s,  6, ] <- c(rep(0,  6), state_after_move[p1p, p1s,  6, -(26:31)])
      state[p1p, p1s,  7, ] <- c(rep(0,  7), state_after_move[p1p, p1s,  7, -(25:31)])
      state[p1p, p1s,  8, ] <- c(rep(0,  8), state_after_move[p1p, p1s,  8, -(24:31)])
      state[p1p, p1s,  9, ] <- c(rep(0,  9), state_after_move[p1p, p1s,  9, -(23:31)])
      state[p1p, p1s, 10, ] <- c(rep(0, 10), state_after_move[p1p, p1s, 10, -(22:31)])
    }
  }
  p2_wins <- p2_wins + sum(state[, , , 22:31])
  state[, , , 22:31] <- 0
  turn <- turn + 1L
}
format(max(p1_wins, p2_wins), scientific = FALSE) # part two: 131888061854776
