x <- readLines("14.txt")
init <- strsplit(x[1], "")[[1]]
productions <- x[-(1:2)]
predecessors <- substr(productions, 1, 2)
split_predecessors <- do.call(cbind, strsplit(predecessors, ""))
insertions <- substr(productions, 7, 7)

init_pairs <- paste0(init[-length(init)], init[-1])
successors1 <- paste0(substr(productions, 1, 1), insertions)
successors2 <- paste0(insertions, substr(productions, 2, 2))
stopifnot(identical(substr(successors1, 2, 2), substr(successors2, 1, 1)))

production_pairs <- c(
  predecessors,
  successors1,
  successors2
)
possible_pairs <- sort(unique(c(init_pairs, production_pairs)))
len <- length(possible_pairs)

init_pair_counts <- tabulate(
  factor(init_pairs, levels = possible_pairs),
  nbins = len
)
last_letter <- init[length(init)]
predecessor_indices <- match(predecessors, possible_pairs)
successor_indices <- rbind(
  match(successors1, possible_pairs),
  match(successors2, possible_pairs)
)[, order(predecessor_indices)]
pair_first_letters <- substr(possible_pairs, 1, 1)
letters <- sort(unique(c(last_letter, pair_first_letters)))

iterate <- function(counts) {
  new_counts <- rep(0L, len)
  for (n in 1:len) {
    new_counts[successor_indices[1, n]] <- new_counts[successor_indices[1, n]] + counts[n]
    new_counts[successor_indices[2, n]] <- new_counts[successor_indices[2, n]] + counts[n]
  }
  new_counts
}

current <- init_pair_counts
for (i in 1:10) {
  current <- iterate(current)
}
tally <- rep(0L, length(letters))
for (i in seq.int(len)) {
  tally[match(pair_first_letters[i], letters)] <-
    tally[match(pair_first_letters[i], letters)] +
    current[i]
}
tally[match(last_letter, letters)] <-
  tally[match(last_letter, letters)] +
  1L
diff(range(tally)) # part one: 4517

current2 <- as.numeric(current) # too large for integers
for (i in 11:40) {
  current2 <- iterate(current2)
}
tally2 <- rep(0L, length(letters))
for (i in seq.int(len)) {
  tally2[match(pair_first_letters[i], letters)] <-
    tally2[match(pair_first_letters[i], letters)] +
    current2[i]
}
tally2[match(last_letter, letters)] <-
  tally2[match(last_letter, letters)] +
  1L
format(diff(range(tally2)), scientific = FALSE) # part two: 4704817645083
