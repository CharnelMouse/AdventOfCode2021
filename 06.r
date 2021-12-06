x <- scan("06.txt", sep = ",", quiet = TRUE)
start_freqs <- tabulate(x + 1L, nbins = 9)

# Could rewrite as a finite different equation with analytic solution,
# but I can't be bothered
freqs <- start_freqs
new_freqs <- freqs
for (i in 1:80) {
  new_freqs[1] <- freqs[2]
  new_freqs[2] <- freqs[3]
  new_freqs[3] <- freqs[4]
  new_freqs[4] <- freqs[5]
  new_freqs[5] <- freqs[6]
  new_freqs[6] <- freqs[7]
  new_freqs[7] <- freqs[8] + freqs[1]
  new_freqs[8] <- freqs[9]
  new_freqs[9] <- freqs[1]
  freqs <- new_freqs
}
sum(freqs) # part one: 377263

# need gmp here for super-double integers
freqs2 <- gmp::as.bigz(freqs)
new_freqs <- freqs2
for (i in 81:256) {
  new_freqs[1] <- freqs2[2]
  new_freqs[2] <- freqs2[3]
  new_freqs[3] <- freqs2[4]
  new_freqs[4] <- freqs2[5]
  new_freqs[5] <- freqs2[6]
  new_freqs[6] <- freqs2[7]
  new_freqs[7] <- freqs2[8] + freqs2[1]
  new_freqs[8] <- freqs2[9]
  new_freqs[9] <- freqs2[1]
  freqs2 <- new_freqs
}
sum(freqs2) # part two: 1695929023803
