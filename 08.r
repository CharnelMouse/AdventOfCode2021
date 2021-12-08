x <- scan("08.txt", rep(list(character()), 15))
patterns <- do.call(cbind, x[1:10])
outputs <- do.call(cbind, x[12:15])
n <- nrow(outputs)

# unique lengths:
# 1: 2
# 4: 4
# 7: 3
# 8: 7

sum(nchar(outputs) %in% c(2L, 4L, 3L, 7L)) # part one: 445

# use a-g for given letters, A-G for real letters

keys <- array(1L, dim = c(7, 7, n), dimnames = list(LETTERS[1:7], letters[1:7], NULL))
numbers <- c(
  "ABCEFG",
  "CF",
  "ACDEG",
  "ACDFG",
  "BCDF",
  "ABDFG",
  "ABDEFG",
  "ACF",
  "ABCDEFG",
  "ABCDFG"
)

# just one set for now
# patterns[1,]
# outputs[1,]
# key <- keys[, , 1]
# both <- c(patterns[1, ], outputs[1, ])
# needed <- sort(Reduce(union, strsplit(outputs[1,], "", fixed = TRUE)))
# ones <- strsplit(both[nchar(both) == 2][1], "", fixed = TRUE)[[1]]

# lazy, very slow brute force

perm <- function(x) {
  y <- unname(do.call(expand.grid, c(rep(list(x), length(x)), KEEP.OUT.ATTRS = FALSE)))
  y[apply(y, 1, Negate(anyDuplicated)), ]
}
maps <- apply(perm(LETTERS[1:7]), 1, paste, collapse = "")

solve_single <- function(n) {
  for (i in 1:length(maps)) {
    dec_both <- chartr("abcdefg", maps[i], c(patterns[n, ], outputs[n, ]))
    dec_both_sorted <- strsplit(dec_both, "", fixed = TRUE) |>
      vapply(\(x) paste(sort(x), collapse = ""), character(1))
    matches <- match(dec_both_sorted, numbers)
    if (!anyNA(matches)) break
  }
  as.integer(sum((matches[11:14] - 1L)*10L^(3:0)))
}
solutions <- vapply(seq.int(n), solve_single, integer(1))
sum(solutions) # part two: 1043101
