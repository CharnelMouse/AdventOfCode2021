x <- scan("06.txt", sep = ",", quiet = TRUE)
start_freqs <- tabulate(x + 1L, nbins = 9)

# mostly freqs just cycle, so
# we track a rolling index instead of
# cycling the vector around.
# loop i is current position of 0-day fish,
# so add this to what would be
# position (i+7) mod 9 under 0-indexing.
freqs <- start_freqs
for (i in (0:79) %% 9L + 1L) {
  day6_pos <- (i + 6L) %% 9L + 1L
  freqs[day6_pos] <- freqs[day6_pos] + freqs[i]
}
sum(freqs) # part one: 377263

# need gmp here for super-double integers
freqs2 <- gmp::as.bigz(freqs)
for (i in (80:255) %% 9L + 1L) {
  day6_pos <- (i + 6L) %% 9L + 1L
  freqs2[day6_pos] <- freqs2[day6_pos] + freqs2[i]
}
sum(freqs2) # part two: 1695929023803
