minPayCalc <- function(n, interest, loan) {
  n <- n*12    # should be in months but input is in years
  interest <- interest/(12*100) # APR needs to be converted from a yearly rate
  discount <- (((1 + interest)^n) - 1)/(interest*(1 + interest)^n)
  payment <- loan/discount
  round(payment, 2)
}

currentLoanValue <- function(initialLoan, interest, n, payment) {
  n <- n*12
  current.val <- rep(NA, n)
  current.val[1] <- initialLoan
  interest <- interest/(12*100)
  for (i in 2:n) {
    prev.val <- current.val[i-1]
    current.val[i] <- prev.val-payment+interest*prev.val    
  }
  current.val
}