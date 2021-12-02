x <- scan("02.txt", list(dir = character(), n = integer()), quiet = TRUE)

aim_history <- cumsum(x$n*((x$dir == "down") - (x$dir == "up")))
forward <- which(x$dir == "forward")
forward_ns <- x$n[forward]

pos <- sum(forward_ns)
pos*aim_history[length(aim_history)] # part one: 1947824

depth <- sum(forward_ns*aim_history[forward])
pos*depth # part two: 1813062561
