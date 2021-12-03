x <- readLines("03.txt")
bitmat <- strsplit(x, "") |>
  lapply(strtoi) |>
  do.call(what = rbind)
modes <- colSums(bitmat) > nrow(bitmat)/2
to_num <- function(modes) {
  len <- length(modes)
  sum(modes*2^((len - 1):0))
}
gamma <- to_num(modes)
epsilon <- 2^ncol(bitmat) - 1 - gamma
gamma*epsilon # part one: 775304

rating <- function(bitmat, op, index = 1) {
  if (nrow(bitmat) == 1)
    drop(bitmat)
  else{
    criterion <- op(sum(bitmat[, index]), nrow(bitmat)/2)
    rating(
      bitmat[bitmat[, index] == criterion, , drop = FALSE],
      op,
      index + 1
    )
  }
}
oxygen <- to_num(rating(bitmat, `>=`))
co2 <- to_num(rating(bitmat, `<`))
oxygen*co2 # part two: 1370737
