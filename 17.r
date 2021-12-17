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
# (t - (v+1/2))^2 <= (v+1/2)^2 - 2ymin,
# (t - (v+1/2))^2 >= (v+1/2)^2 - 2ymax.
# If ymax <= 0, every term is positive, so we require
# [sqrt((v+1/2)^2 - 2ymax) - 1/2, sqrt((v+1/2)^2 - 2ymin) - 1/2]
# to contain an integer.
# If z is contained, then (z+1/2)^2 is in
# [(v+1/2)^2 - 2ymax, (v+1/2)^2 - 2ymin], so
# (z+v+1)(z-v) is in [2(-ymax), 2(-ymin)].
# Let z = v+y, then (2v+y+1)y is in [2(-ymax), 2(-ymin)].
# So v must at least satisfy 2v+2 <= 2(-ymin), i.e.
# v <= (-ymin)-1. So we can count down from there.
# v = (-ymin)-1 satisifies the equality, so is always a solution:
# the probe goes up, returns to 0 height, then moves v+1 down.
# Count down to ymin, to get all possible velocities for part two.

max_vely <- -range_y[1] - 1L
max_vely*(max_vely + 1L)/2 # part one: 6441

vely <- max_vely
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
# Minimum velocity just reaches xmin, so
# v(v+1)/2 >= xmin => v >= -1/2 + 1/2 sqrt(8xmin + 1).

minvx <- ceiling(-1/2 + sqrt(8*range_x[1] + 1L)/2)
validy <- integer()
nvalid <- 0L

for (n in seq_along(maybevalidy)) {
  for (vx in minvx:range_x[2]) {
    s <- min(mintimesy[n], vx):min(maxtimesy[n], vx)
    posx <- s*(vx - (s - 1)/2)
    nvalid <- nvalid + any(posx >= range_x[1] & posx <= range_x[2])
  }
}
nvalid # part two: 3186
