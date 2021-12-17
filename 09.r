x <- readLines("09.txt") |>
  strsplit("", fixed = TRUE) |>
  lapply(as.integer) |>
  do.call(what = rbind)

r <- nrow(x)
c <- ncol(x)
local_minima <-
  cbind(x[, -c] < x[, -1], TRUE) &
  cbind(TRUE, x[, -1] < x[, -c]) &
  rbind(x[-r, ] < x[-1, ], TRUE) &
  rbind(TRUE, x[-1, ] < x[-r, ])
n_minima <- sum(local_minima)
sum(x[local_minima]) + n_minima # part one: 448

ids <- matrix(seq_along(x), nrow = r)
left <- cbind(Inf, x[, -c])
right <- cbind(x[, -1], Inf)
up <- rbind(Inf, x[-r, ])
down <- rbind(x[-1, ], Inf)
moves <- array(c(x, left, right, up, down), dim = c(r, c, 5L))
downhill <- apply(
  moves,
  1:2,
  \(vals) {
    if (vals[1] == 9)
      -1L
    else{
      w <- which.min(vals)
      if (w == 1L) NA else w
    }
  }
)
stopifnot(sum(is.na(downhill)) == n_minima)
basins <- matrix(NA, nrow = r, ncol = c)
basins[local_minima] <- seq.int(n_minima)
basins[x == 9L] <- -1L

for (i in seq.int(r)) {
  for (j in seq.int(c)) {
    if (!is.element(downhill[i, j], c(-1L, NA_integer_))) {
      row <- i
      col <- j
      n <- downhill[row, col]
      while (!is.na(n)) {
        switch(
          n,
          stop("shouldn't be here"),
          col <- col - 1L,
          col <- col + 1L,
          row <- row - 1L,
          row <- row + 1L
        )
        n <- downhill[row, col]
      }
      basins[i, j] <- basins[row, col]
    }
  }
}
top3 <- n_minima - 0:2
prod(sort(tabulate(basins[basins > 0]), partial = top3)[top3]) # part two: 1417248
