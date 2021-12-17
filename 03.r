x <- readLines("03.txt")
bitmat <- strsplit(x, "", fixed = TRUE) |>
  lapply(strtoi) |>
  do.call(what = rbind)
n <- nrow(bitmat)
modes <- colSums(bitmat) > n/2
to_num <- function(modes) {
  len <- length(modes)
  sum(modes*2^((len - 1):0))
}
gamma <- to_num(modes)
epsilon <- 2^ncol(bitmat) - 1 - gamma
gamma*epsilon # part one: 775304

rating <- function(op, indices = seq.int(n), index = 1) {
  if (length(indices) == 1)
    bitmat[indices, ]
  else{
    bits <- bitmat[, index]
    criterion <- op(sum(bits), n/2)
    rating(
      op,
      indices[bits[indices] == criterion],
      index + 1
    )
  }
}
oxygen <- to_num(rating(`>=`))
co2 <- to_num(rating(`<`))
oxygen*co2 # part two: 1370737
