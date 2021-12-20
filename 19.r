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
  # each of three axes can become x, -x, y, -y, z, or -z
  # in some transformation, so we check each of the 3x6
  # pairs to avoid redundancy from checking all transformations.
  points1 <- do.call(rbind, lst1)
  points2 <- do.call(rbind, lst2)
  axes2 <- cbind(points2, -points2)
  element_differences <- array(
    NA_integer_,
    dim = c(nrow(points1), nrow(points2), 3L, 6L)
  )
  for (axis_label1 in 1:3) {
    for (axis_label2 in 1:6) {
      element_differences[, , axis_label1, axis_label2] <-
        outer(points1[, axis_label1], axes2[, axis_label2], `-`)
    }
  }
  max_axis_matches <- apply(
    element_differences,
    3:4,
    \(x) {
      fac <- factor(x)
      freqs <- tabulate(fac)
      nm <- levels(fac)[freqs >= 12]
      n <- length(nm)
      stopifnot(n <= 1)
      if (n == 0)
        NA_integer_
      else
        as.integer(nm)
    }
  )
  # Only ever three valid axis pairings, so no
  # complex choice-making for which transformation to use.
  # (Input-specific?)
  stopifnot(sum(!is.na(max_axis_matches)) <= 3)
  if (
    all(apply(max_axis_matches, 1, \(x) any(!is.na(x)))) &&
    (any(!is.na(max_axis_matches[, 1])) || any(!is.na(max_axis_matches[, 4]))) &&
    (any(!is.na(max_axis_matches[, 2])) || any(!is.na(max_axis_matches[, 5]))) &&
    (any(!is.na(max_axis_matches[, 3])) || any(!is.na(max_axis_matches[, 6])))
  ) {
    indices <- apply(max_axis_matches, 1, \(x) which(!is.na(x)))
    diffs <- apply(max_axis_matches, 1, na.omit)
    new_points2 <- cbind(
      axes2[, indices[1]] + diffs[1],
      axes2[, indices[2]] + diffs[2],
      axes2[, indices[3]] + diffs[3]
    )
    list(
      apply(new_points2, 1, identity, simplify = FALSE),
      diffs
    )
  }
  else
    NULL
}

unmatched_scanners <- seq.int(n_scanners)[-1]
matched_hits <- hits[1]
n_unmatched <- length(unmatched_scanners)
new_matched_scanners <- 1L
current <- 2L
scanner_positions <- list(c(0L, 0L, 0L))

while (n_unmatched > 0) {
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
          next_matched_scanners <- c(next_matched_scanners, current)
          current <- current + 1L
          flag <- FALSE
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
