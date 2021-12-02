times <- microbenchmark::microbenchmark(
  source("01.r"),
  source("02.r"),
  setup = expression(rm(list = ls()))
)

time_summary <- function(times, scale = 1) {
  data.table::as.data.table(times)[
    ,
    .(
      mean = mean(time/scale),
      sd = sd(time/scale),
      se = sd(time/scale)/.N,
      `low (2.5%)` = quantile(time/scale, 0.025),
      `high (97.5%)` = quantile(time/scale, 0.975)
    ),
    by = expr
  ]
}
