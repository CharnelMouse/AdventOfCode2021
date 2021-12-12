x <- scan("12.txt", list(character(), character()), sep = "-", quiet = TRUE)
connections <- data.frame(from = c(x[[1]], x[[2]]), to = c(x[[2]], x[[1]]))

paths <- function(location, connections) {
  if (location == "end")
    return(1L)
  destinations <- connections$to[connections$from == location]
  if (length(destinations) == 0L)
    return(0L)
  new_connections <- if (location == tolower(location))
    connections[connections$from != location & connections$to != location, , drop = FALSE]
  else
    connections
  sum(vapply(destinations, paths, integer(1), new_connections))
}
paths("start", connections) # part one: 3708

locations <- unique(connections$from)
small_location_indices <- which(
  !is.element(locations, c("start", "end")) &
    locations == tolower(locations)
)
visits_left <- ifelse(locations == tolower(locations), 2, Inf)
visits_left[match(c("start", "end"), locations)] <- 1
remove_finished <- function(connections, visits_left) {
  finished_locations <- locations[visits_left <= 0]
  connections[
    !is.element(connections$from, finished_locations) &
      !is.element(connections$to, finished_locations),
    ,
    drop = FALSE
  ]
}
paths2 <- function(location, connections, visits_left, visit2) {
  if (location == "end")
    return(1L)
  location_index <- match(location, locations)
  if (
    location != "start" &&
    location == tolower(location) &&
    visits_left[location_index] == 1 &&
    !visit2
  ) {
    other_small <- setdiff(small_location_indices, location_index)
    visits_left[other_small] <- visits_left[other_small] - 1
    visit2 <- TRUE
  }
  connections <- remove_finished(connections, visits_left)
  destinations <- connections$to[connections$from == location]
  if (length(destinations) == 0L)
    return(0L)
  visits_left[location_index] <- visits_left[location_index] - 1
  sum(vapply(destinations, paths2, integer(1), connections, visits_left, visit2))
}
paths2("start", connections, visits_left, FALSE) # part two: 93858
