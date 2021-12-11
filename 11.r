x <- scan("11.txt", character(), quiet = TRUE) |>
  strsplit("", fixed = TRUE) |>
  vapply(as.integer, integer(10)) |>
  t()

ids <- matrix(seq.int(100), nrow = 10, ncol = 10)
neighbours <- array(
  c(
    rbind(NA, ids[-10, ]),
    rbind(ids[-1, ], NA),
    cbind(NA, ids[, -10]),
    cbind(ids[, -1], NA),
    cbind(NA, rbind(NA, ids[-10, -10])),
    cbind(NA, rbind(ids[-1, -10], NA)),
    cbind(rbind(NA, ids[-10, -1]), NA),
    cbind(rbind(ids[-1, -1], NA), NA)
  ),
  dim = c(100, 8)
)

y <- x
total_flashes <- 0L
for (t in 1:100) {
  y <- y + 1L
  flashes <- which(y > 9L)
  new_flashes <- flashes
  while (length(new_flashes) > 0L) {
    affected <- na.omit(as.integer(neighbours[new_flashes, ]))
    for (n in affected) {
      y[n] <- y[n] + 1L
    }
    new_flashes <- setdiff(which(y > 9L), flashes)
    flashes <- c(flashes, new_flashes)
  }
  total_flashes <- total_flashes + length(flashes)
  y[y > 9L] <- 0L
}
total_flashes # part one: 1667

y <- x
flashes <- integer()
t <- 0L
while (length(flashes) < 100L) {
  t <- t + 1L
  y <- y + 1L
  flashes <- which(y > 9L)
  new_flashes <- flashes
  while (length(new_flashes) > 0L) {
    affected <- na.omit(as.integer(neighbours[new_flashes, ]))
    for (n in affected) {
      y[n] <- y[n] + 1L
    }
    new_flashes <- setdiff(which(y > 9L), flashes)
    flashes <- c(flashes, new_flashes)
  }
  y[y > 9L] <- 0L
}
t # part two: 488
