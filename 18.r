# Adding two reduced numbers can't result
# in a single number being at depth 5+,
# so we can ignore the "pair" part of the
# explode condition.
# This lets us store numbers as flat vectors
# of the values and depths.

x <- strsplit(readLines("18.txt"), "", fixed = TRUE)

parse_number <- function(chars) {
  values <- integer()
  depths <- integer()
  depth <- 0L
  for (c in chars) {
    n <- strtoi(c)
    if (is.na(n)) {
      if (c == "[")
        depth <- depth + 1L
      if (c == "]")
        depth <- depth - 1L
    }else{
      values <- c(values, n)
      depths <- c(depths, depth)
    }
  }
  list(values = values, depths = depths)
}

numbers <- lapply(x, parse_number)

reduce_once <- function(number) {
  first_deep <- match(TRUE, number$depths >= 5L)
  if (!is.na(first_deep)) {
    # explode
    new_values <- number$values
    new_values[first_deep - 1L] <- new_values[first_deep - 1L] +
      new_values[first_deep]
    if (first_deep + 1L < length(new_values)) {
      new_values[first_deep + 2L] <- new_values[first_deep + 2L] +
        new_values[first_deep + 1L]
    }
    new_values <- c(
      new_values[0:(first_deep - 1L)],
      0L,
      new_values[-seq.int(first_deep + 1L)]
    )
    new_depths <- c(
      number$depths[0:(first_deep - 1L)],
      number$depths[first_deep] - 1L,
      number$depths[-seq.int(first_deep + 1L)]
    )
    list(values = new_values, depths = new_depths)
  }else{
    first_big <- match(TRUE, number$values >= 10L)
    if (!is.na(first_big)) {
      # split
      list(
        values = c(
          number$values[0:(first_big - 1L)],
          floor(number$values[first_big]/2),
          ceiling(number$values[first_big]/2),
          number$values[-seq.int(first_big)]
        ),
        depths = c(
          number$depths[0:(first_big - 1L)],
          rep(number$depths[first_big] + 1L, 2),
          number$depths[-seq.int(first_big)]
        )
      )
    }else{
      number
    }
  }
}

reduce_number <- function(number) {
  new <- reduce_once(number)
  while (!identical(number, new)) {
    number <- new
    new <- reduce_once(number)
  }
  number
}

add_numbers <- function(n1, n2) {
  pre_reduced <- list(
    values = c(n1$values, n2$values),
    depths = c(n1$depths, n2$depths) + 1L
  )
  reduce_number(pre_reduced)
}

magnitude <- function(number) {
  len <- length(number$values)
  while (len > 1) {
    first_pair <- match(
      TRUE,
      number$depths[-len] == number$depths[-1]
    )
    number <- list(
      values = c(
        number$values[0:(first_pair - 1L)],
        3L*number$values[first_pair] + 2L*number$values[first_pair + 1L],
        number$values[-seq.int(first_pair + 1L)]
      ),
      depths = c(
        number$depths[0:(first_pair - 1L)],
        number$depths[first_pair] - 1L,
        number$depths[-seq.int(first_pair + 1L)]
      )
    )
    len <- len - 1L
  }
  number$values
}

numbers_sum <- Reduce(add_numbers, numbers)
magnitude(numbers_sum) # part one: 3734

pair_indices <- subset(
  expand.grid(seq_along(numbers), seq_along(numbers)),
  Var1 != Var2
)

max_pair_sum_mag <- -Inf
for (i in seq.int(nrow(pair_indices))) {
  max_pair_sum_mag <- max(
    max_pair_sum_mag,
    magnitude(
      add_numbers(numbers[[pair_indices[i, 1]]], numbers[[pair_indices[i, 2]]])
    )
  )
}
max_pair_sum_mag # part two: 4837
