x <- scan("08.txt", rep(list(character()), 15), quiet = TRUE)
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

perm <- function(x) {
  y <- unname(do.call(expand.grid, c(rep(list(x), length(x)), KEEP.OUT.ATTRS = FALSE)))
  y[!apply(y, 1, anyDuplicated), , drop = FALSE]
}
# slow, should be done separately for each line so I can filter first
maps <- perm(LETTERS[1:7])

string_perms <- function(chars) {
  unname(apply(perm(chars), 1, paste, collapse = ""))
}
one_patterns <- string_perms(c("C", "F"))
seven_patterns <- string_perms(c("A", "C", "F"))
four_patterns <- string_perms(c("B", "C", "D", "F"))

filter_maps <- function(maps, strings, char_len, patterns) {
  match_index <- match(char_len, nchar(strings), 0L)
  if (!match_index)
    maps
  else{
    given_char_nums <- match(strsplit(strings[match_index], "")[[1]], letters[1:7])
    chars <- maps[, given_char_nums]
    strs <- apply(
      chars,
      1,
      paste,
      collapse = ""
    )
    maps[strs %in% patterns, , drop = FALSE]
  }
}

solve_single <- function(index) {
  strings <- c(patterns[index, ], outputs[index, ])
  local_maps <- maps |>
    filter_maps(strings, 2L, one_patterns) |>
    filter_maps(strings, 3L, seven_patterns) |>
    filter_maps(strings, 4L, four_patterns)
  local_map_strings <- apply(local_maps, 1, paste, collapse = "")
  for (i in 1:length(local_map_strings)) {
    dec_both <- chartr("abcdefg", local_map_strings[i], strings)
    dec_both_sorted <- strsplit(dec_both, "", fixed = TRUE) |>
      vapply(\(x) paste(sort(x), collapse = ""), character(1))
    matches <- match(dec_both_sorted, numbers)
    if (!anyNA(matches)) break
  }
  if (anyNA(matches))
    stop(paste(
      "Unresolved line:",
      toString(strings),
      "Filtered maps:",
      toString(local_map_strings),
      "One:",
      one_patterns[1],
      "Seven:",
      seven_patterns[1],
      "Four:",
      four_patterns[1],
      sep = "\n"
    ))
  as.integer(sum((matches[11:14] - 1L)*10L^(3:0)))
}
solutions <- vapply(seq.int(n), solve_single, integer(1))
sum(solutions) # part two: 1043101
