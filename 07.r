x <- scan("07.txt", integer(), sep = ",", quiet = TRUE)

# Absolute distance is minimised by the median.
align_point <- round(median(x))
sum(abs(x - align_point)) # part one: 347449

# Cost for point m is the average of
# the square distance and the absolute distance,
# so we expect the optimal point m to lie
# between the mean and the median.
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
