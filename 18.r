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
    new_number <- number
    new_number$values[first_deep - 1L] <- new_number$values[first_deep - 1L] +
      new_number$values[first_deep]
    if (first_deep + 1L < length(new_number$values)) {
      new_number$values[first_deep + 2L] <- new_number$values[first_deep + 2L] +
        new_number$values[first_deep + 1L]
    }
    new_number$values <- c(
      new_number$values[0:(first_deep - 1L)],
      0L,
      new_number$values[-seq.int(first_deep + 1L)]
    )
    new_number$depths <- c(
      new_number$depths[0:(first_deep - 1L)],
      new_number$depths[first_deep] - 1L,
      new_number$depths[-seq.int(first_deep + 1L)]
    )
  }else{
    first_big <- match(TRUE, number$values >= 10L)
    if (!is.na(first_big)) {
      # split
      new_number <- list(
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
      if (length(new_number$values) != length(new_number$depths))
        stop(paste("error in split", print(number), print(first_big), print(new_number)))
    }else{
      new_number <- number
    }
  }
  new_number
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

magnitude_once <- function(number) {
  len <- length(number$values)
  index <- 1L
  new_number <- list(values = integer(), depths = integer())
  flag <- FALSE
  while (index <= len) {
    if (index < len && number$depths[index + 1L] == number$depths[index] && !flag) {
      new_number$values <- c(
        new_number$values,
        3L*number$values[index] + 2L*number$values[index + 1L]
      )
      new_number$depths <- c(new_number$depths, number$depths[index] - 1L)
      flag <- TRUE
      index <- index + 2L
    }else{
      new_number$values <- c(new_number$values, number$values[index])
      new_number$depths <- c(new_number$depths, number$depths[index])
      index <- index + 1L
    }
  }
  new_number
}

magnitude <- function(number) {
  while (length(number$values) > 1)
    number <- magnitude_once(number)
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
