#fxn_DiffMetrics.R
#Make functions to calculate the difference metrics for each obsID: Hedges d and lnRR


### Hedges' d (Hedges and Olkin 1985) ####################################################
#NOTES
# Mean values can have different signs

# MEAN
# d = (Y1 - Y2) * J / sqrt( ( (N1-1)S1^2 + (N2-1)S2^2 ) / (N1 + N2 - 2) )
# J = 1 - ( 3 / 4*(N1 + N2 - 2) - 1)

# VAR
# vd = ( (N1 + N2) / (N1 * N2) ) + ( d^2 / (2*(N1 + N2)) )

# where...
# d = Hedges' d
# vd = variance for Hedges' d
# Y1 and Y2 = mean estimates from 1 obsID
# N1 and N2 = counts of samples associated with means 1 and 2 from obsID
# S1 and S2 = standard devidations associated with means 1 and 2 from obsID

HedgesD<-function(Y1, Y2, S1, S2, N1, N2){
  
  # MEAN
  J <- 1 - ( 3 / 4 * (N1 + N2 - 2) - 1)
  d <- (Y1 - Y2) * J / sqrt( ( (N1 - 1) * S1^2 + (N2 - 1) * S2^2 ) / (N1 + N2 - 2) )
  
  # VAR
  vd <- ( (N1 + N2) / (N1 * N2) ) + ( d^2 / (2 * (N1 + N2)) )
  
  # Save results
  results<-list(d=d, vd=vd)
  return(results)
}




### Natural log response ratio, R (Hedges et al 1999) ####################################################
# NOTES
# This doesn't work for situations where the mean values have different signs 
# Usually report the response ratio as the back-transformed values of ln R after the analyses are done, since the ratio is easier to interpret than the log-transformed value

# MEAN
# ln R = ln(Y1 / Y2) = ln(Y1) - ln(Y2)

# VAR
# vlnR = (S1^2 / N1*Y1^2) + (S2^2 / N2*Y2^2)

# where...
# ln R = natural log of the response ratio
# vlnR = variance for the natural log of the response ratio
# Y1 and Y2 = mean estimates from 1 obsID
# N1 and N2 = counts of samples associated with means 1 and 2 from obsID
# S1 and S2 = standard devidations associated with means 1 and 2 from obsID

LnResponseR<-function(Y1, Y2, S1, S2, N1, N2){
  
  # MEAN
  lnR <- log(Y1 / Y2) #log() does the natural log (ln)
  
  # VAR
  vlnR <- (S1^2 / N1 * Y1^2) + (S2^2 / N2 * Y2^2)
  
  # Save results
  results<-list(lnR=lnR, vlnR=vlnR)
  return(results)
  
}









