x <- readLines("13.txt")
gap <- which(x == "")
points <- scan(
  "13.txt",
  nmax = gap - 1L,
  list(x = integer(), y = integer()),
  sep = ",",
  quiet = TRUE
) |>
  as.data.frame()
n_points <- nrow(points)
folds <- substring(x[(gap + 1L):length(x)], 12)
fold_dirs <- substr(folds, 1, 1)
fold_pos <- as.integer(substring(folds, 3))

points[[fold_dirs[1]]] <- ifelse(
  points[[fold_dirs[1]]] < fold_pos[1],
  points[[fold_dirs[1]]],
  2*fold_pos[1] - points[[fold_dirs[1]]]
)
n_points - sum(duplicated(points)) # part one: 735

for (n in 2:length(fold_dirs)) {
  points[[fold_dirs[n]]] <- ifelse(
    points[[fold_dirs[n]]] < fold_pos[n],
    points[[fold_dirs[n]]],
    2*fold_pos[n] - points[[fold_dirs[n]]]
  )
}

show <- matrix(FALSE, nrow = max(points$y) + 1L, ncol = max(points$x) + 1L)
for (n in seq.int(nrow(points))) {
  show[points$y[n] + 1L, points$x[n] + 1L] <- TRUE
}
cat(ifelse(t(show), "#", " "), fill = ncol(show), sep = "") # part two: UFRZKAUZ
