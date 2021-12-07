x <- scan("07.txt", integer(), sep = ",", quiet = TRUE)

# Absolute distance is minimised by the median.
align_point <- round(median(x))
sum(abs(x - align_point)) # part one: 347449

# Cost for point m is the average of
# the square distance and the absolute distance,
# so we expect the optimal point m to lie
# between the mean and the median.
# Moving away from the mean increases
# square distances by at least len, and
# decreases absolute distances by at most
# len. So we don't need to check positions
# very far from the mean.
# (I think only the mean itself rounded
# towards the median, and the one after
# that if the mean is integer, but it
# takes longer to find those specific ones)
mn <- mean(x)
possible <- (floor(mn) - 1L):(ceiling(mn) + 1L)
costs <- vapply(
  possible,
  \(val) {
    a <- abs(x - val)
    sum(a*(1 + a))/2
  },
  double(1)
)
min(costs) # part two: 98039527
