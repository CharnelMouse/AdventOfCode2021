# Speed notes
# I think the main cause of slowness here is using
# recursion for paths/paths2, so first priority
# would be converting to an iterative version.
# Secondly, I think I can remove the big caves
# before I start, e.g. if we have
# a -> A
# A -> a
# A -> b
# then we'd replace this with
# a -> a
# a -> b
# This removes the need to check the cave type
# before changing the remaining number of visits.
# We could also remove a -> a transitions for part
# one, since we know in advance they can't be used.

x <- scan("12.txt", list(character(), character()), sep = "-", quiet = TRUE)

locations <- unique(c(x[[1]], x[[2]]))
small_location_indices <- which(
  !is.element(locations, c("start", "end")) &
    locations == tolower(locations)
)
connections <- matrix(
  FALSE,
  nrow = length(locations),
  ncol = length(locations),
  dimnames = list(locations, locations)
)
for (n in seq_along(x[[1]])) {
  connections[x[[1]][n], x[[2]][n]] <- TRUE
  connections[x[[2]][n], x[[1]][n]] <- TRUE
}

start_index <- match("start", locations)
end_index <- match("end", locations)
paths <- function(location, connections) {
  if (location == end_index)
    return(1L)
  destinations <- which(connections[location, ])
  if (length(destinations) == 0L)
    return(0L)
  if (location %in% c(start_index, small_location_indices)) {
    connections[location, ] <- FALSE
    connections[, location] <- FALSE
  }
  sum(vapply(destinations, paths, integer(1), connections))
}
paths(start_index, connections) # part one: 3708

visits_left <- ifelse(locations == tolower(locations), 2, Inf)
visits_left[c(start_index, end_index)] <- 1
paths2 <- function(location, connections, visits_left, visit2) {
  if (location == end_index)
    return(1L)
  if (
    location %in% small_location_indices &&
    visits_left[location] == 1 &&
    !visit2
  ) {
    other_small <- setdiff(small_location_indices, location)
    visits_left[other_small] <- visits_left[other_small] - 1
    visit2 <- TRUE
  }
  finished_locations <- visits_left <= 0
  connections[finished_locations, ] <- FALSE
  connections[, finished_locations] <- FALSE
  destinations <- which(connections[location, ])
  if (length(destinations) == 0L)
    return(0L)
  visits_left[location] <- visits_left[location] - 1
  s <- 0L
  for (n in destinations) {
    s <- s + paths2(n, connections, visits_left, visit2)
  }
  s
}
paths2(start_index, connections, visits_left, FALSE) # part two: 93858
