x <- strtoi(readLines("1.txt"))
len <- length(x)
sum(x[-1] > x[-len]) # part one: 1581
sum(x[-(1:3)] > x[-(len - 0:2)]) # part two: 1618
