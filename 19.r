x <- readLines("19.txt")
x <- x[x != ""]
scanner_header_indices <- startsWith(x, "--")
n_scanners <- sum(scanner_header_indices)
scanner_vals <- cumsum(scanner_header_indices)[!scanner_header_indices]
hits <- split(
  apply(
    unname(as.data.frame(scan(
      text = paste(x[!scanner_header_indices], collapse = "\n"),
      what = list(integer(), integer(), integer()),
      sep = ",",
      quiet = TRUE
    ))),
    1,
    identity,
    simplify = FALSE
  ),
  scanner_vals
)

match_scanners <- function(lst1, lst2) {
  n1 <- length(lst1)
  n2 <- length(lst2)
  # We shouldn't be checking all the transforms in isolation.
  # We should be checking 1, -1, 2, -2, 3, and -3 for matches
  # and using those to determine the transformation.
  transforms <- list(
    \(x) c( x[1],  x[2],  x[3]),
    \(x) c(-x[1], -x[2],  x[3]),
    \(x) c( x[1], -x[2], -x[3]),
    \(x) c(-x[1],  x[2], -x[3]),
    \(x) c(-x[1],  x[3],  x[2]),
    \(x) c( x[1], -x[3],  x[2]),
    \(x) c( x[1],  x[3], -x[2]),
    \(x) c(-x[1], -x[3], -x[2]),
    \(x) c(-x[2],  x[1],  x[3]),
    \(x) c( x[2], -x[1],  x[3]),
    \(x) c( x[2],  x[1], -x[3]),
    \(x) c(-x[2], -x[1], -x[3]),
    \(x) c( x[2],  x[3],  x[1]),
    \(x) c(-x[2], -x[3],  x[1]),
    \(x) c( x[2], -x[3], -x[1]),
    \(x) c(-x[2],  x[3], -x[1]),
    \(x) c( x[3],  x[1],  x[2]),
    \(x) c(-x[3], -x[1],  x[2]),
    \(x) c( x[3], -x[1], -x[2]),
    \(x) c(-x[3],  x[1], -x[2]),
    \(x) c(-x[3],  x[2],  x[1]),
    \(x) c( x[3], -x[2],  x[1]),
    \(x) c( x[3],  x[2], -x[1]),
    \(x) c(-x[3], -x[2], -x[1])
  )
  for (tr in transforms) {
    lst3 <- lapply(lst2, tr)
    diffs <- list()
    for (i1 in seq.int(n1)) {
      for (i2 in seq.int(n2)) {
        diffs <- c(diffs, list(lst3[[i2]] - lst1[[i1]]))
      }
    }
    diffs <- unique(diffs)
    for (df in diffs) {
      shifted2 <- lapply(lst3, `-`, df)
      intersection <- intersect(lst1, shifted2)
      if (length(intersection) >= 12)
        return(list(shifted2, df))
    }
  }
  NULL
}

matched_scanners <- 1L
unmatched_scanners <- seq.int(n_scanners)[-1]
matched_hits <- hits[1]
n_matched <- 1L
n_unmatched <- length(unmatched_scanners)
new_matched_scanners <- 1L
n_non_new <- 0L
current <- 2L
scanner_positions <- list(c(0L, 0L, 0L))

# Very long! c. half an hour
while (n_unmatched > 0) {
  cat("looping...\n")
  next_matched_scanners <- integer()
  for (scanner in unmatched_scanners) {
    flag <- TRUE
    for (matched_scanner in new_matched_scanners) {
      if (flag) {
        mch <- match_scanners(matched_hits[[matched_scanner]], hits[[scanner]])
        if (!is.null(mch)) {
          unmatched_scanners <- setdiff(unmatched_scanners, scanner)
          matched_hits <- c(matched_hits, list(mch[[1]]))
          scanner_positions <- c(scanner_positions, list(mch[[2]]))
          n_unmatched <- n_unmatched - 1L
          matched_scanners <- c(matched_scanners, scanner)
          next_matched_scanners <- c(next_matched_scanners, current)
          current <- current + 1L
          flag <- FALSE
          cat(n_unmatched, "\n")
        }
      }
    }
  }
  if (length(next_matched_scanners) == 0)
    stop("no match")
  new_matched_scanners <- next_matched_scanners
}
combined_hits <- unique(do.call(c, matched_hits))
length(combined_hits) # part one: 442

max_dist <- -Inf
for (h1 in scanner_positions) {
  for (h2 in scanner_positions) {
    max_dist <- max(max_dist, sum(abs(h1 - h2)))
  }
}
max_dist # part two: 11079
