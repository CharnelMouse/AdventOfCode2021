# Using A* again

x <- readLines("23.txt")
# x <- c(
#   "#############",
#   "#...........#",
#   "###B#C#B#D###",
#   "  #A#D#C#A#  ",
#   "  #########  "
# )
corridor_length <- nchar(x[2]) - 2L
side_length <- length(x) - 3L
side_split <- strsplit(x[3:(length(x) - 1)], "", fixed = TRUE)
side_positions <- which(side_split[[1]] != "#") - 1L
n_sides <- length(side_positions)
side_starts <- side_split |>
  vapply(\(chars) chars[side_positions + 1L], character(n_sides))
cell_costs <- c(1L, 10L, 100L, 1000L)
labels <- LETTERS[1:4]

corridor_starts <- rep(NA_character_, corridor_length)

side_filled <- function(sides) {
  filled <- rep(0L, n_sides)
  for (n in 1:n_sides) {
    fail <- match(
      FALSE,
      rev(!is.na(sides[n, ]) & sides[n, ] == labels[n]),
      nomatch = side_length + 1L
    )
    filled[n] <- fail - 1L
  }
  filled
}

heuristic <- function(corridor, sides) {
  filled <- side_filled(sides)
  to_fill <- side_length - filled
  move_in <- as.integer(sum(cell_costs*to_fill*(to_fill + 1L) / 2L))
  move_out <- 0L
  move_along <- 0L
  for (c in seq.int(corridor_length)[!is.na(corridor)]) {
    # cat("corridor ", c, ": ")
    leaver_label_match <- match(corridor[c], labels)
    leaver_cell_cost <- cell_costs[leaver_label_match]
    # cat("cell cost ", toString(leaver_cell_costs), " ")
    along_dist <- sum(
      leaver_cell_cost*abs(c - side_positions[leaver_label_match]),
      na.rm = TRUE
    )
    # cat("along distance ", along_dist, "\n")
    move_along <- move_along + along_dist
  }
  for (s in (1:n_sides)[to_fill > 0]) {
    # cat("side ", s, ": ")
    # cat(toString(n_leave), " leavers ")
    leaver_label_matches <- match(sides[s, 1:to_fill[s]], labels)
    # cat("with matches ", toString(leaver_label_matches), " ")
    leaver_cell_costs <- cell_costs[leaver_label_matches]
    # cat("cell costs ", toString(leaver_cell_costs), "\n")
    move_out <- move_out +
      sum((1:to_fill[s]) * leaver_cell_costs, na.rm = TRUE)
    move_along <- move_along +
      sum(
        leaver_cell_costs*pmin(2L, abs(side_positions[s] - side_positions[leaver_label_matches])),
        na.rm = TRUE
      )
  }
  stopifnot(length(move_out) == 1, length(move_along) == 1, length(move_in) == 1)
  # cat("out: ", move_out, " along: ", move_along, " in: ", move_in, "\n")
  move_out + move_along + move_in
}

corridor_hash <- function(corridor) {
  sum((5^(seq.int(corridor_length) - 1))*match(corridor, labels, nomatch = 0))
}
sides_hash <- function(sides) {
  sum((5^(seq.int(n_sides*side_length) - 1))*match(sides, labels, nomatch = 0))
}

corridor_unhash <- function(hash) {
  rem <- hash
  corridor <- character()
  for (n in (length(labels) + 1)^rev((seq.int(corridor_length) - 1L))) {
    corridor <- c(c(NA, labels)[1 + rem %/% n], corridor)
    rem <- rem %% n
  }
  stopifnot(rem == 0)
  corridor
}
sides_unhash <- function(hash) {
  rem <- hash
  sides <- matrix(NA, nrow = length(labels), ncol = side_length)
  for (n in (seq.int(length(sides)) - 1L)) {
    md <- (length(labels) + 1)^(length(sides) - n - 1)
    sides[length(sides) - n] <- c(NA, labels)[1 + rem %/% md]
    rem <- rem %% md
  }
  stopifnot(rem == 0)
  sides
}

moves <- function(corridor, sides, debug = FALSE) {
  filled <- side_filled(sides)
  to_fill <- side_length - filled
  move_sides <- list()
  move_corridors <- list()
  move_costs <- integer()
  # mvs <- character()
  for (c in seq_along(corridor)[!is.na(corridor)]) {
    if (debug) cat("corridor space ", c, ": ")
    id <- match(corridor[c], labels)
    if (debug) cat(corridor[c] ," id ", id, " ")
    # only considering moves into own side room
    if (
      sum(!is.na(sides[id, ])) == filled[id] && # side room doesn't have aliens
      all(is.na(corridor[c:side_positions[id]][-1])) # corridor isn't blocked
    ) {
      if (debug) cat("side room available ")
      distance <- abs(side_positions[id] - c) + to_fill[id]
      move_corridors <- c(move_corridors, list(`[<-`(corridor, c, NA_character_)))
      move_sides <- c(move_sides, list(`[<-`(sides, id, to_fill[id], corridor[c])))
      cost <- distance*cell_costs[id]
      move_costs <- c(move_costs, cost)
      # mvs <- c(mvs, paste0(corridor[c], " C", c, "->S", id, "[", side_positions[id], "],", to_fill[id], " dist ", distance, " cost ", cost))
    }
    if (debug) cat("finished\n")
  }
  for (s in seq.int(n_sides)) {
    if (debug) cat("side room ", s, " ")
    if (
      filled[s] < side_length && # side room not completed
      sum(!is.na(sides[s, ])) > filled[s] # aliens present
    ) {
      alien_pos <- min(match(labels, sides[s, ]), na.rm = TRUE)
      alien_label <- sides[s, alien_pos]
      if (debug) cat("alien ", alien_label, " at ", alien_pos, " ")
      left_corridor <- corridor[side_positions[s]:1][-1]
      first_left_blocked <- side_positions[s] - min(match(labels, left_corridor, nomatch = length(left_corridor) + 1L))
      right_corridor <- corridor[side_positions[s]:corridor_length][-1]
      first_right_blocked <- side_positions[s] + min(match(labels, right_corridor, nomatch = length(right_corridor) + 1L))
      if (first_right_blocked - first_left_blocked > 2L) { # spaces available
        if (debug) cat("corridor space available ")
        available <- setdiff(
          (first_left_blocked + 1L):(first_right_blocked - 1L),
          side_positions # can't stop outside side rooms
        )
        for (target in available) {
          move_corridors <- c(move_corridors, list(`[<-`(corridor, target, alien_label)))
          move_sides <- c(move_sides, list(`[<-`(sides, s, alien_pos, NA_character_)))
          distance <- alien_pos + abs(target - side_positions[s])
          cost <- cell_costs[match(alien_label, labels)] * distance
          move_costs <- c(
            move_costs,
            cost
          )
          # mvs <- c(mvs, paste0(alien_label, " S", s, "[", side_positions[s], "],", alien_pos, "->C", target, " dist ", distance, " cost ", cost))
        }
      }
    }
    if (debug) cat("finished\n")
  }
  list(
    corridor = move_corridors,
    side = move_sides,
    cost = move_costs,
    ch = sapply(move_corridors, corridor_hash),
    sh = sapply(move_sides, sides_hash)#,
    # mv = mvs
  )
}

solve <- function() {
  cache <- list(
    corridor = corridor_hash(corridor_starts),
    side = sides_hash(side_starts)
  )
  move_list <- moves(corridor_starts, side_starts)
  cache$corridor <- c(cache$corridor, move_list$ch)
  cache$side <- c(cache$side, move_list$sh)
  stopifnot(length(cache$corridor) == length(cache$side))
  move_list$ch <- lapply(move_list$ch, \(h) c(corridor_hash(corridor_starts), h))
  move_list$sh <- lapply(move_list$sh, \(h) c(sides_hash(side_starts), h))
  heuristics <- mapply(heuristic, move_list$corridor, move_list$side)
  stopifnot(is.integer(heuristics))
  stopifnot(length(move_list$cost) == length(heuristics))
  nxt <- which.min(move_list$cost + heuristics)
  nxt_sides <- move_list$side[[nxt]]
  nxt_cost <- move_list$cost[nxt]
  nxt_corridor <- move_list$corridor[[nxt]]
  nxt_ch <- move_list$ch[[nxt]]
  nxt_sh <- move_list$sh[[nxt]]
  nxt_mv <- move_list$mv[[nxt]]
  nxt_tot <- nxt_cost + heuristics[nxt]
  n_stuck <- 0L

  while (!identical(side_filled(nxt_sides), rep(side_length, n_sides))) {
    cat("\r", nxt_tot)
    new_move_list <- moves(nxt_corridor, nxt_sides)
    rem <- integer()
    for (n in seq_along(new_move_list$ch)) {
      ch_matches <- cache$corridor == new_move_list$ch[n]
      if (any(cache$side[ch_matches] == new_move_list$sh[n])) {
        rem <- c(rem, n)
      }
    }
    if (length(rem) > 0) {
      new_move_list$corridor <- new_move_list$corridor[-rem]
      new_move_list$side <- new_move_list$side[-rem]
      new_move_list$cost <- new_move_list$cost[-rem]
      new_move_list$ch <- new_move_list$ch[-rem]
      new_move_list$sh <- new_move_list$sh[-rem]
      # new_move_list$mv <- new_move_list$mv[-rem]
    }
    cache$corridor <- c(cache$corridor, new_move_list$ch)
    cache$side <- c(cache$side, new_move_list$sh)
    stopifnot(length(cache$corridor) == length(cache$side))
    if (length(new_move_list$cost) > 0) {
      new_move_list$cost <- new_move_list$cost + nxt_cost
      # new_move_list$mv <- lapply(new_move_list$mv, \(h) c(nxt_mv, h))
      move_list$corridor <- c(move_list$corridor[-nxt], new_move_list$corridor)
      move_list$side <- c(move_list$side[-nxt], new_move_list$side)
      move_list$cost <- c(move_list$cost[-nxt], new_move_list$cost)
      move_list$ch <- c(move_list$ch[-nxt], new_move_list$ch)
      move_list$sh <- c(move_list$sh[-nxt], new_move_list$sh)
      # move_list$mv <- c(move_list$mv[-nxt], new_move_list$mv)
      heuristics <- c(
        heuristics[-nxt],
        mapply(heuristic, new_move_list$corridor, new_move_list$side)
      )
    }else{
      n_stuck <- n_stuck + 1L
      move_list$corridor <- move_list$corridor[-nxt]
      move_list$side <- move_list$side[-nxt]
      move_list$cost <- move_list$cost[-nxt]
      move_list$ch <- move_list$ch[-nxt]
      move_list$sh <- move_list$sh[-nxt]
      # move_list$mv <- move_list$mv[-nxt]
      heuristics <- heuristics[-nxt]
    }
    stopifnot(length(move_list$cost) > 0)
    stopifnot(is.integer(heuristics))
    stopifnot(length(move_list$cost) == length(heuristics))
    nxt <- which.min(move_list$cost + heuristics)
    nxt_sides <- move_list$side[[nxt]]
    nxt_cost <- move_list$cost[nxt]
    nxt_corridor <- move_list$corridor[[nxt]]
    nxt_ch <- move_list$ch[[nxt]]
    nxt_sh <- move_list$sh[[nxt]]
    # nxt_mv <- move_list$mv[[nxt]]
    stopifnot(nxt_cost + heuristics[nxt] >= nxt_tot)
    nxt_tot <- nxt_cost + heuristics[nxt]
  }

  stopifnot(heuristics[nxt] == 0)
  nxt_cost
}

solve() # part one: 14346

x <- c(
  x[1:3],
  "  #D#C#B#A#  ",
  "  #D#B#A#C#  ",
  x[4:5]
)
# x <- c(
#   "#############",
#   "#...........#",
#   "###B#C#B#D###",
#   "  #D#C#B#A#  ",
#   "  #D#B#A#C#  ",
#   "  #A#D#C#A#  ",
#   "  #########  "
# )
corridor_length <- nchar(x[2]) - 2L
side_length <- length(x) - 3L
side_split <- strsplit(x[3:(length(x) - 1)], "", fixed = TRUE)
side_positions <- which(side_split[[1]] != "#") - 1L
n_sides <- length(side_positions)
side_starts <- side_split |>
  vapply(\(chars) chars[side_positions + 1L], character(n_sides))

solve() # part two: 48984
