x <- readLines("22.txt")
n_cubes <- length(x)
ons <- startsWith(x, "on")
cubes <- strsplit(x, ",", fixed = TRUE) |>
  lapply(\(str) {
    unlist(strsplit(str, "=", fixed = TRUE))[c(2, 4, 6)] |>
      strsplit("..", fixed = TRUE) |>
      unlist() |>
      as.numeric()
  })

# If we work backwards from the last instruction/cube,
# We don't need to track cube states, just
# whether its last mention is "on".
# We only need to track total count, so for each "on" cube
# we find
# ||cube1 & !(cube2 | cube3 | ...)||,
# where cube2 etc. are the later cubes.
# tracking unions is a pain, since we need to
# break up into several smaller cubes.
# However, we can do
# ||cube1|| - ||cube1 & (cube2 | cube3 | ...)||
# = ||cube1|| - ||(cube1 & cube2) | (cube1 & cube3) | ...)||

cube_intersect <- function(cubes) {
  mins <- apply(vapply(cubes, `[`, numeric(3), c(1, 3, 5)), 1, max)
  maxs <- apply(vapply(cubes, `[`, numeric(3), c(2, 4, 6)), 1, min)
  c(mins[1], maxs[1], mins[2], maxs[2], mins[3], maxs[3])
}

cube_size <- function(cube) {
  if (cube[2] < cube[1] || cube[4] < cube[3] || cube[6] < cube[5])
    0
  else
    (cube[2] + 1 - cube[1])*(cube[4] + 1 - cube[3])*(cube[6] + 1 - cube[5])
}

pairwise_collisions <- outer(
  seq.int(n_cubes),
  seq.int(n_cubes),
  Vectorize(\(n1, n2) {
    if (n2 >= n1)
      FALSE
    else
      cube_size(cube_intersect(cubes[c(n1, n2)])) > 0
  })
)

cube_minus_cubes <- function(cube, cubes_list) {
  n_int <- length(cubes_list)
  if (n_int == 0)
    return(cube_size(cube))
  start_intersections <- lapply(cubes_list, \(c) cube_intersect(list(c, cube)))
  widths <- 1:n_int
  total <- 0
  for (width in widths) {
    sign <- (-1)^(width - 1)
    combs <- combn(widths, width)
    intersection_sizes <- apply(
      combs,
      2,
      \(c) cube_size(cube_intersect(start_intersections[c]))
    )
    total <- total + sign*sum(intersection_sizes)
  }
  cube_size(cube) - total
}

simple <- vapply(
  cubes,
  \(x) x[1] >= -50 && x[2] <= 50 &&
    x[3] >= -50 && x[4] <= 50 &&
    x[5] >= -50 && x[6] <= 50,
  logical(1)
)
simple_cubes <- cubes[simple]
simple_ons <- ons[simple]
simple_collisions <- pairwise_collisions[simple, simple]

count <- 0
for (n in rev(which(simple_ons))) {
  count <- count +
    cube_minus_cubes(simple_cubes[[n]], simple_cubes[simple_collisions[, n]])
}
format(count, scientific = FALSE) # part one: 542711

count <- 0
for (n in (n_cubes:1)[rev(ons)]) {
  count <- count + cube_minus_cubes(cubes[[n]], cubes[pairwise_collisions[, n]])
}
format(count, scientific = FALSE) # part two: 1160303042684776
