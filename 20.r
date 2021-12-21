# 2D equivalent to Wolfram rules, like Game of Life.

rule <- strsplit(readLines("20.txt", 1), "", fixed = TRUE)[[1]] == "#"
start <- scan("20.txt", character(1), skip = 2, quiet = TRUE) |>
  strsplit("", fixed = TRUE) |>
  sapply(`==`, "#") |>
  t()

step <- function(state) {
  grid <- state[[1]]
  background <- state[[2]] # value of cells not stored explicitly
  r <- nrow(grid)
  c <- ncol(grid)
  b <- as.numeric(background)
  big_grid <- cbind(b, b, rbind(b, b, grid, b, b), b, b)
  indices <-
    2^8*big_grid[1:(r + 2), 1:(c + 2)] +
    2^7*big_grid[1:(r + 2), 2:(c + 3)] +
    2^6*big_grid[1:(r + 2), 3:(c + 4)] +
    2^5*big_grid[2:(r + 3), 1:(c + 2)] +
    2^4*big_grid[2:(r + 3), 2:(c + 3)] +
    2^3*big_grid[2:(r + 3), 3:(c + 4)] +
    2^2*big_grid[3:(r + 4), 1:(c + 2)] +
    2^1*big_grid[3:(r + 4), 2:(c + 3)] +
    2^0*big_grid[3:(r + 4), 3:(c + 4)]
  list(
    matrix(rule[indices + 1L], nrow = r + 2L, ncol = c + 2L),
    rule[511*b + 1L]
  )
}

state <- step(step(list(start, FALSE)))
stopifnot(!state[[2]]) # check non-stored cells are dark
sum(state[[1]]) # part one: 4928

for (n in 3:50) {
  state <- step(state)
}
stopifnot(!state[[2]])
sum(state[[1]]) # part two: 16605
