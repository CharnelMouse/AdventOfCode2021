x <- readLines("14.txt")
init <- strsplit(x[1], "", fixed = TRUE)[[1]]
productions <- x[-(1:2)]
predecessors <- substr(productions, 1, 2)
insertions <- substr(productions, 7, 7)

init_pairs <- paste0(init[-length(init)], init[-1])
successors1 <- paste0(substr(predecessors, 1, 1), insertions)
successors2 <- paste0(insertions, substr(predecessors, 2, 2))
possible_pairs <- sort(unique(c(
  init_pairs,
  predecessors,
  successors1,
  successors2
)))
len <- length(possible_pairs)

successor_indices <- rbind(
  match(successors1, possible_pairs),
  match(successors2, possible_pairs)
)[, order(predecessors)] # reorder here so don't need predecessor index lookups

iterate <- function(counts) {
  new_counts <- rep(0L, len)
  for (n in 1:len) {
    # could use tapply here, would need to use factors with a default
    new_counts[successor_indices[1, n]] <- new_counts[successor_indices[1, n]] +
      counts[n]
    new_counts[successor_indices[2, n]] <- new_counts[successor_indices[2, n]] +
      counts[n]
  }
  new_counts
}

last_letter <- init[length(init)]
pair_first_letters <- substr(possible_pairs, 1, 1)
letters <- sort(unique(c(last_letter, pair_first_letters)))

tallydiff <- function(counts) {
  tally <- rep(0L, length(letters))
  for (i in seq.int(len)) {
    tally[match(pair_first_letters[i], letters)] <-
      tally[match(pair_first_letters[i], letters)] +
      counts[i]
  }
  tally[match(last_letter, letters)] <-
    tally[match(last_letter, letters)] +
    1L
  diff.default(range.default(tally))
}

current <- tabulate(match(init_pairs, possible_pairs), nbins = len)
for (i in 1:10) {
  current <- iterate(current)
}
tallydiff(current) # part one: 4517

current2 <- as.numeric(current) # too large for integers
for (i in 11:40) {
  current2 <- iterate(current2)
}
format(tallydiff(current2), scientific = FALSE) # part two: 4704817645083
