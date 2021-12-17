x <- strsplit(
  substring(readLines("17.txt"), nchar("target area: x=") + 1),
  ", ",
  fixed = TRUE
)[[1]]
range_x <- as.integer(strsplit(x[1], "..", fixed = TRUE)[[1]])
range_y <- as.integer(
  strsplit(
    substring(x[2], nchar("y=") + 1),
    "..",
    fixed = TRUE
  )[[1]]
)

# given initial speed v on y-axis, position at
# time t is vt - t(t-1)/2. So we need t such that
# ymin <= vt - t(t-1)/2 <= ymax, i.e.
# t(t - (2v+1)) + 2ymin <= 0,
# t(t - (2v+1)) + 2ymax <= 0.
# or
# (t - (v+1/2))^2 <= (v+1/2)^2 - 2ymin,
# (t - (v+1/2))^2 >= (v+1/2)^2 - 2ymax.
# If ymax <= 0, every term is positive, so we require
# [sqrt((v+1/2)^2 - 2ymax) - 1/2, sqrt((v+1/2)^2 - 2ymin) - 1/2]
# to contain an integer.
# If z is contained, then (z+1/2)^2 is in
# [(v+1/2)^2 - 2ymax, (v+1/2)^2 - 2ymin], so
# (z+v)(z-v) is in [2(-ymax), 2(-ymin)].
# Let z = v+y, then (2v+y)y is in [2(-ymax), 2(-ymin)].
# So v must at least satisfy 2v+1 <= 2(-ymin), or
# v <= (-ymin)-1. So we can count down from there.
# Count down to ymin, to get all possible velocities for part two.

vely <- -range_y[1] - 1
maybevalidy <- integer()
maxtimesy <- integer()
mintimesy <- integer()

while (vely >= range_y[1]) {
  soly_centre <- vely + 1/2
  soly_range <- soly_centre + sqrt((soly_centre)^2 - 2*range_y)
  maxtimey <- floor(soly_range[1])
  mintimey <- ceiling(soly_range[2])
  if (maxtimey >= mintimey) {
    maybevalidy <- c(maybevalidy, vely)
    maxtimesy <- c(maxtimesy, maxtimey)
    mintimesy <- c(mintimesy, mintimey)
  }
  vely <- vely - 1L
}

# For a range [t1, t2] of times where the probe is
# vertically aligned with the target area, we need
# to find matching horizontal velocities.
# Velocity at time t, given starting velocity v, is
# (v - t)[t <= v], so position is
# (vs - s(s-1)/2), where s = min(t, v).

validy <- integer()
validx <- integer()

for (n in seq_along(maybevalidy)) {
  for (vx in 1:range_x[2]) {
    s <- min(mintimesy[n], vx):min(maxtimesy[n], vx)
    posx <- s*(vx - (s - 1)/2)
    if (any(posx >= range_x[1] & posx <= range_x[2])) {
      validx <- c(validx, vx)
      validy <- c(validy, maybevalidy[n])
    }
  }
}
validy[1]*(validy[1] + 1)/2 # part one: 6441
length(validy) # part two: 3186
