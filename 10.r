x <- readLines("10.txt")

single_pass <- function(strs) {
  strs |>
    gsub(pattern = "()", replacement = "", fixed = TRUE) |>
    gsub(pattern = "[]", replacement = "", fixed = TRUE) |>
    gsub(pattern = "{}", replacement = "", fixed = TRUE) |>
    gsub(pattern = "<>", replacement = "", fixed = TRUE)
}
simplify <- function(strs) {
  new <- single_pass(strs)
  while (!identical(strs, new)) {
    strs <- new
    new <- single_pass(strs)
  }
  strs
}
simp <- simplify(x)
illegal <- rbind(
  unlist(regexpr(")", simp, fixed = TRUE)),
  unlist(regexpr("]", simp, fixed = TRUE)),
  unlist(regexpr("}", simp, fixed = TRUE)),
  unlist(regexpr(">", simp, fixed = TRUE))
)
illegal[illegal == -1] <- NA
illegal_points <- apply(
  illegal,
  2,
  \(x) {
    w <- which.min(x)
    if (length(w) == 0)
      NA
    else
      switch(
        w,
        3L,
        57L,
        1197L,
        25137L
      )
  }
)
sum(illegal_points, na.rm = TRUE) # part one: 316851

simp[is.na(illegal_points)] |>
  gsub(pattern = "(", replacement = "1", fixed = TRUE) |>
  gsub(pattern = "[", replacement = "2", fixed = TRUE) |>
  gsub(pattern = "{", replacement = "3", fixed = TRUE) |>
  gsub(pattern = "<", replacement = "4", fixed = TRUE) |>
  strsplit("") |>
  vapply(\(x) sum(as.integer(x)*5^(0:(length(x) - 1L))), numeric(1)) |>
  median.default() # part two: 2182912364
