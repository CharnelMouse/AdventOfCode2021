x <- scan("06.txt", sep = ",", quiet = TRUE)
start_freqs <- tabulate(x + 1L, nbins = 9)

# mostly freqs just cycle, so
# we track a rolling index instead of
# cycling the vector around.
# Under 0-indexing,
# loop i mod 9 is current position of 0-day fish,
# so add this to 6-day fish, i.e.
# position (i+7) mod 9.
# R has 1-indexing, so need to adjust this a bit.
freqs <- start_freqs
day6_positions <- (1:9 + 6L) %% 9L + 1L
for (i in (0:79) %% 9L + 1L) {
  day6_pos <- day6_positions[i]
  freqs[day6_pos] <- freqs[day6_pos] + freqs[i]
}
sum(freqs) # part one: 377263

# need gmp here for super-double integers
freqs2 <- gmp::as.bigz(freqs)
for (i in (80:255) %% 9L + 1L) {
  day6_pos <- day6_positions[i]
  freqs2[day6_pos] <- freqs2[day6_pos] + freqs2[i]
}
sum(freqs2) # part two: 1695929023803
