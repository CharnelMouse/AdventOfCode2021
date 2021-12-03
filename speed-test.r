times <- microbenchmark::microbenchmark(
  source("01.r"),
  source("02.r"),
  source("03.r"),
  setup = expression(rm(list = ls()))
)

times
print(times, "relative")
