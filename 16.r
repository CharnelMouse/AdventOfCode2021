x <- readLines("16.txt")

to_bin <- function(str) {
  hex_digits <- strsplit(str, "")[[1]]
  sapply(
    hex_digits,
    function(char) {
      switch(
        char,
        `0` = c(FALSE, FALSE, FALSE, FALSE),
        `1` = c(FALSE, FALSE, FALSE, TRUE ),
        `2` = c(FALSE, FALSE, TRUE , FALSE),
        `3` = c(FALSE, FALSE, TRUE , TRUE ),
        `4` = c(FALSE, TRUE , FALSE, FALSE),
        `5` = c(FALSE, TRUE , FALSE, TRUE ),
        `6` = c(FALSE, TRUE , TRUE , FALSE),
        `7` = c(FALSE, TRUE , TRUE , TRUE ),
        `8` = c(TRUE , FALSE, FALSE, FALSE),
        `9` = c(TRUE , FALSE, FALSE, TRUE),
        `A` = c(TRUE , FALSE, TRUE , FALSE),
        `B` = c(TRUE , FALSE, TRUE , TRUE),
        `C` = c(TRUE , TRUE , FALSE, FALSE),
        `D` = c(TRUE , TRUE , FALSE, TRUE ),
        `E` = c(TRUE , TRUE , TRUE , FALSE),
        `F` = c(TRUE , TRUE , TRUE , TRUE )
      )
    }
  ) |>
    as.logical()
}
bin <- to_bin(x)

to_num <- function(bools) {
  sum(bools*2^((length(bools) - 1):0))
}

parse_versions <- function(bin) {
  if (length(bin) <= 6)
    return(list(version_sum = 0L, remaining = bin))
  version <- to_num(bin[1:3])
  type <- to_num(bin[4:6])
  if (type == 4) {
    index <- 7L
    value_bin <- logical()
    while (bin[index] == 1) {
      value_bin <- c(value_bin, bin[index + 1:4])
      index <- index + 5L
    }
    value_bin <- c(value_bin, bin[index + 1:4])
    value <- to_num(value_bin)
    list(
      version_sum = version,
      value = value,
      remaining = bin[-(1:(index + 4L))]
    )
  }else{
    op <- switch(
      type + 1L,
      sum,
      prod,
      min,
      max,
      stop("literal passed as op"),
      `>`,
      `<`,
      `==`
    )
    if (bin[7]) {
      n_sub <- to_num(bin[8:18])
      if ((type == 5 || type == 6 || type == 7) && n_sub != 2)
        stop("comparison op given ", n_sub, " inputs")
      remaining_bits <- bin[-(1:18)]
      values <- numeric(n_sub)
      for (n in 1:n_sub) {
        nxt <- parse_versions(remaining_bits)
        version <- version + nxt$version_sum
        values[n] <- nxt$value
        remaining_bits <- nxt$remaining
      }
    }else{
      rem_length <- to_num(bin[8:22])
      remaining_bits <- bin[-(1:22)]
      values <- numeric()
      start_rem <- length(remaining_bits)
      while (start_rem - length(remaining_bits) < rem_length) {
        nxt <- parse_versions(remaining_bits)
        version <- version + nxt$version_sum
        values <- c(values, nxt$value)
        remaining_bits <- nxt$remaining
      }
    }
    value <- do.call(op, as.list(values))
    list(
      version_sum = version,
      value = value,
      remaining = remaining_bits
    )
  }
}

res <- parse_versions(bin)
res$version_sum # part one: 897
format(res$value, scientific = FALSE) # part two: 9485076995911
