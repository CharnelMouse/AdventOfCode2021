x <- scan("07.txt", integer(), sep = ",", quiet = TRUE)

# Absolute distance is minimised by the median.
align_point <- round(median(x))
sum(abs(x - align_point)) # part one: 347449

# Cost for point m is the average of
# the square distance and the absolute distance,
# so we expect the optimal point m to lie
# between the mean and the median.
# We expect it to be much closer to the mean,
# since the square distance has more influence,
# so we could start at the mean and iterate
# towards the median, breaking when the cost
# begins to increase. However, in R this is
# slower than calculating all the costs in
# the interval.
mn <- mean(x)
range_min <- floor(min(mn, align_point))
range_max <- ceiling(max(mn, align_point))
possible <-  range_min:range_max

costs <- vapply(
  possible,
  \(val) {
    a <- abs(x - val)
    sum(a*(1 + a))/2
  },
  double(1)
)
min(costs) # part two: 98039527
