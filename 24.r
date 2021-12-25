x <- readLines("24.txt")

# Doing what might be some input-specific stuff,
# so here are a bunch of checks.
# inp commands are evenly spaced and split the
# code into evenly-sized chunks, with the
# same ops and first arguments in each chunk.
ops <- substr(x, 1, 3)
inp_dists <- diff(which(ops == "inp"))
chunk_size <- inp_dists[1]
n_chunks <- length(inp_dists) + 1L
stopifnot(all(inp_dists == chunk_size))
stopifnot(length(x) == n_chunks*chunk_size)
chunk_ops <- ops[1:chunk_size]
stopifnot(identical(ops, rep(chunk_ops, n_chunks)))

instr <- strsplit(x, " ", fixed = TRUE)

first_args <- sapply(instr, `[`, 2)
stopifnot(identical(first_args, rep(first_args[1:chunk_size], n_chunks)))

# Chunk pattern:
# inp w
# mul x 0
# add x z
# mod x 26
# div z a
# add x b
# eql x w
# eql x 0
# mul y 0
# add y 25
# mul y x
# add y 1
# mul z y
# mul y 0
# add y w
# add y c
# mul y x
# add z y

# w is just used to store inputs.
# x and y are reset to zero in each chunk, so no inter-chunk state
# except for z.
# x -> [z mod 26 + b != w], y -> 25[z mod 26 + b != w] + 1
# z -> (z/a)*(25[z mod 26 + b != i] + 1) + [z mod 26 + b != i](i + c),
# where i is the input.

second_args <- matrix(sapply(instr, `[`, 3), nrow = chunk_size, ncol = n_chunks)
stopifnot(
  all(second_args[ 2, ] == "0"),
  all(second_args[ 3, ] == "z"),
  all(second_args[ 4, ] == "26"),
  all(second_args[ 7, ] == "w"),
  all(second_args[ 8, ] == "0"),
  all(second_args[ 9, ] == "0"),
  all(second_args[10, ] == "25"),
  all(second_args[11, ] == "x"),
  all(second_args[12, ] == "1"),
  all(second_args[13, ] == "y"),
  all(second_args[14, ] == "0"),
  all(second_args[15, ] == "w"),
  all(second_args[17, ] == "x"),
  all(second_args[18, ] == "y")
)

a <- as.integer(second_args[5, ])
b <- as.integer(second_args[6, ])
c <- as.integer(second_args[16, ])

# indicator false: z -> z/a
# indicator true: 26(z/a) + i + c.
# Fastest reduction is z/a. For final z to be 0,
# z should be less than product of remaining a's.

a_prods <- c(rev(cumprod(rev(a)))[-1], 1)

z <- 0

for (chunk in 1:n_chunks) {
  cat("\r", chunk, " ", length(z), "     ")
  # (z/a)*(25[z mod 26 + b != i] + 1) + [z mod 26 + b != i](i + c)
  z <- setNames(
    rep(z, each = 9),
    paste0(rep(names(z), each = 9), 9:1)
  )
  indicators <- z %% 26 + b[chunk] != rep_len(9:1, length(z))
  if (a[chunk] != 1)
    z <- trunc(z/a[chunk])
  z <- z*(25*indicators + 1) + indicators*(9:1 + c[chunk])
  z <- z[z < a_prods[chunk]]
  z <- z[!duplicated(z) | !duplicated(z, fromLast = TRUE)]
}

names(z)[1] # part one: 39924989499969
names(z)[2] # part two: 16811412161117
