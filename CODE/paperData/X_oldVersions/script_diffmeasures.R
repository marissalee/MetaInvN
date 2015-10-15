#script_logRRmeasures.R
#Calculate the weighted mean difference for each measurement


### Load Fxns and Libraries ####################################################
source('code/paperData/fxn_ConvertVar.R') #file with fxn to convert to variance to SD
source('code/paperData/fxn_DiffMetrics.R') #file with fxn to convert to calc hedges' d and LnRR





### Check data ####################################################
#str(measures)



### Convert Var to SD ####################################################
#SD (standard deviation) = square.root(VAR (variance))
v1 <- measures$measInvVar_VAR
v2 <- measures$measNatVar_VAR
measures$measInvVar_SD<-VARtoSD(v1)
measures$measNatVar_SD<-VARtoSD(v2)




### Assign parameters and start a dataframe ####################################################
Y1 <- measures$measInvMean #use the non-standardized column, because units are irrelevant and you get more data
Y2 <- measures$measNatMean
S1 <- measures$measInvVar_SD #use the newly-calculated standard deviation
S2 <- measures$measNatVar_SD
N1 <- measures$measInvN
N2 <- measures$measNatN
n<-apply(measures[,c('measInvN','measNatN')], 1, min)


dfES<-data.frame(obsID=measures$obsID,measCat=measures$measCat,
           Y1, Y2, 
           S1, S2,
           N1, N2, n)

### Calculate Hedges' d (Hedges and Olkin 1985) ####################################################
#NOTES
# Mean values can have different signs

# 1. Calculate 
result<-HedgesD(Y1=Y1, Y2=Y2, S1=S1, S2=S2, N1=N1, N2=N2)
dfES$d <- result[['d']]
dfES$vd <- result[['vd']]

# 2. Check results and clean
dfES[!is.finite(dfES$d),c('d','vd')]<-c(NA,NA)
#View(dfES)

### Calculate the response ratio R (Hedges et al 1999) ####################################################
# NOTES
# This doesn't work for situations where the mean values have different signs 
# Usually report the response ratio as the back-transformed values of ln R after the analyses are done, since the ratio is easier to interpret than the log-transformed value

# 1. Calculate 
result<-LnResponseR(Y1=Y1, Y2=Y2, S1=S1, S2=S2, N1=N1, N2=N2)
dfES$lnR <- result[['lnR']]
dfES$vlnR <- result[['vlnR']]

# 2. Check results and clean
dfES[!is.finite(dfES$d),c('lnR','vlnR')]<-c(NA,NA)
#View(dfES)

### Add columns back to measures
measures[,c('n','d','vd','lnR','vlnR')]<-dfES[,c('n','d','vd','lnR','vlnR')]
#View(measures)


