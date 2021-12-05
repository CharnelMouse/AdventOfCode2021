x <- sub(" -> ", ",", readLines("05.txt"))
lines <- scan(
  text = x,
  what = list(x0 = integer(), y0 = integer(), x1 = integer(), y1 = integer()),
  sep = ",",
  quiet = TRUE
)

straight <- which(lines$x0 == lines$x1 | lines$y0 == lines$y1)
parse_line <- function(x0, y0, x1, y1) {
  paste(x0:x1, y0:y1, sep = ",")
}

straight_cells <- Map(
  parse_line,
  lines$x0[straight],
  lines$y0[straight],
  lines$x1[straight],
  lines$y1[straight]
) |>
  unlist()
length(unique(straight_cells[duplicated(straight_cells)])) # part one: 4421

cells <- Map(
  parse_line,
  lines$x0,
  lines$y0,
  lines$x1,
  lines$y1
) |>
  unlist()
length(unique(cells[duplicated(cells)])) # part two: 18674
