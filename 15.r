# Speed notes
# This is basic A*. As such,
# most time is spent in which.min to find next current cell,
# since everything's done in unordered vectors.
# To speed things up, I'd want to add a priority queue.
# Letting cells appear multiple times removes the faff
# of removing their old priorities on update, I'd just
# need to check the top element is unvisited when popped.

x <- do.call(
  rbind,
  lapply(strsplit(readLines("15.txt"), "", fixed = TRUE), as.integer)
)
r <- nrow(x)
c <- ncol(x)
n <- r*c

distances <- matrix(Inf, nrow = r, ncol = c)
distances[1, 1] <- 0
simple_dists <- outer((r - 1):0, (c - 1):0, `+`)

unvisited <- rep(TRUE, n)
current <- 1L

while (unvisited[n] && is.finite(distances[current])) {
  neighbours <- c(
    if (current %% r != 0) current + 1L,
    if (current %% r != 1) current - 1L,
    if (current <= n - r) current + r,
    if (current > r) current - r
  )
  neighbours <- neighbours[unvisited[neighbours]]
  new_costs <- distances[current] + x[neighbours]
  distances[neighbours] <- pmin(distances[neighbours], new_costs)
  unvisited[current] <- FALSE
  smallest_unvisited_index <- which.min(
    distances[unvisited] + simple_dists[unvisited]
  )
  current <- which(unvisited)[smallest_unvisited_index]
}
distances[n] # part one: 398

big_map <- rbind(
  cbind(x + 0L, x + 1L, x + 2L, x + 3L, x + 4L),
  cbind(x + 1L, x + 2L, x + 3L, x + 4L, x + 5L),
  cbind(x + 2L, x + 3L, x + 4L, x + 5L, x + 6L),
  cbind(x + 3L, x + 4L, x + 5L, x + 6L, x + 7L),
  cbind(x + 4L, x + 5L, x + 6L, x + 7L, x + 8L)
)
big_map <- (big_map - 1L) %% 9L + 1L

r <- r*5L
c <- c*5L
n <- r*c

distances <- matrix(Inf, nrow = r, ncol = c)
distances[1] <- 0

simple_dists <- outer((r - 1):0, (c - 1):0, `+`)

unvisited <- rep(TRUE, n)
current <- 1L
heuristics <- rep(Inf, n)
heuristics[1] <- simple_dists[1]

# Extremely slow! c. 6 mins
while (unvisited[n] && is.finite(distances[current])) {
  neighbours <- c(
    if (current %% r != 0) current + 1L,
    if (current %% r != 1) current - 1L,
    if (current <= n - r) current + r,
    if (current > r) current - r
  )
  neighbours <- neighbours[unvisited[neighbours]]
  new_costs <- distances[current] + big_map[neighbours]
  distances[neighbours] <- pmin(distances[neighbours], new_costs)
  heuristics[neighbours] <- distances[neighbours] + simple_dists[neighbours]
  unvisited[current] <- FALSE
  smallest_unvisited_index <- which.min(heuristics[unvisited])
  current <- which(unvisited)[smallest_unvisited_index]
}
distances[n] # part two: 2817
