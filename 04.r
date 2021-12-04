calls <- scan("04.txt", sep = ",", nlines = 1, quiet = TRUE)
card_els <- scan("04.txt", integer(5), skip = 2, quiet = TRUE)
cards <- array(card_els, dim = c(5, 5, length(card_els)/25))
called <- array(match(cards, calls), dim = dim(cards))
col_complete <- apply(called, c(1, 3), max)
row_complete <- apply(called, c(2, 3), max)
card_complete <- pmin(apply(col_complete, 2, min), apply(row_complete, 2, min))

winning_card <- which.min(card_complete)
winning_call_index <- card_complete[winning_card]
winning_call <- calls[winning_call_index]
winning_unmarked_sum <- sum(setdiff(cards[, , winning_card], calls[1:winning_call_index]))
winning_call*winning_unmarked_sum # part one: 69579

losing_card <- which.max(card_complete)
losing_call_index <- card_complete[losing_card]
losing_call <- calls[losing_call_index]
losing_unmarked_sum <- sum(setdiff(cards[, , losing_card], calls[1:losing_call_index]))
losing_call*losing_unmarked_sum # part two: 14877
