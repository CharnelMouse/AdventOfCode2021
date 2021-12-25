right_lines <- readLines("25.txt")
r <- length(right_lines)
c <- nchar(right_lines[1])

t_lines <- function(lines) {
  lines |>
    strsplit("", fixed = TRUE) |>
    do.call(what = cbind) |>
    apply(1, paste, collapse = "")
}

old_right <- character(length(right_lines))
turn <- 0

while (any(right_lines != old_right)) {
  turn <- turn + 1
  old_right <- right_lines
  right_wraps <- substr(right_lines, c, c) == ">" & substr(right_lines, 1, 1) == "."
  right_move <- gsub(">.", ".>", right_lines, fixed = TRUE)
  right_move[right_wraps] <- paste0(
    ">",
    substr(right_move[right_wraps], 2, c - 1),
    "."
  )
  down_lines <- t_lines(right_move)
  down_wraps <- substr(down_lines, r, r) == "v" & substr(down_lines, 1, 1) == "."
  down_move <- gsub("v.", ".v", down_lines, fixed = TRUE)
  down_move[down_wraps] <- paste0(
    "v",
    substr(down_move[down_wraps], 2, r - 1),
    "."
  )
  right_lines <- t_lines(down_move)
}

turn # part one: 456
