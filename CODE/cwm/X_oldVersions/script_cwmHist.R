#paperData_summstats/script_cwmHist.R

### Load fxns and libraries ####################################################
source('code/paperData/fxn_loadHelpers.R') #for FixVals.tr
library(ggplot2)
library(reshape2)

### Create a histogram for each std measure, show values that were converted and/or aggregated
cwm_calculated<-ldply(cwm.list, data.frame)
colnames(cwm_calculated)[1]<-'traitCat'
#View(cwm_calculated)

cwm_withReported<-ldply(cwm.r.list, data.frame)
colnames(cwm_withReported)[1]<-'traitCat'
#View(cwm_withReported)

df<-cwm_withReported #use this one
#str(df)



# Melt so that invaded and native values are in the same column
nSpCols<-paste(rep(c('invadedArea','nativeArea'),2),rep(c('invasiveSp','nativeSp'), each=2), sep="_")
idcols<-c('obsID','traitCat','var','nTr','unit','relabund_note',nSpCols,'cwmCalc') 
m.df<-melt(df, id.vars=idcols) #melt
#View(m.df)

#add column to differentiate between inv and nat
m.df$invType<-rep(NA,length(dim(m.df)[1])) 
m.df[grepl('_InvArea',m.df$variable),'invType'] <- 'InvArea'
m.df[grepl('_NatArea',m.df$variable),'invType'] <- 'NatArea'
m.df[grepl('_InvSpInvArea',m.df$variable),'invType'] <- 'InvSpInvArea'
#add column to differentiate between mean, nSp, nMeasCov, nBOSDCov, n1spCov, nXspCov, nOrigTr, nTryGS, nTryGX
m.df$valueType<-rep(NA,length(dim(m.df)[1])) 
m.df[grepl('mean_',m.df$variable),'valueType'] <- 'mean'
m.df[grepl('nSp_',m.df$variable),'valueType'] <- 'nSp'
m.df[grepl('nMeasCov_',m.df$variable),'valueType'] <- 'nMeasCov'
m.df[grepl('nBOSDCov_',m.df$variable),'valueType'] <- 'nBOSDCov'
m.df[grepl('n1spCov_',m.df$variable),'valueType'] <- 'n1spCov'
m.df[grepl('nXspCov_',m.df$variable),'valueType'] <- 'nXspCov'
m.df[grepl('nOrigTr_',m.df$variable),'valueType'] <- 'nOrigTr'
m.df[grepl('nTryGS_',m.df$variable),'valueType'] <- 'nTryGS'
m.df[grepl('nTryGX_',m.df$variable),'valueType'] <- 'nTryGX'
#cast so that each row is an obsID
c.df<-dcast(m.df, obsID + traitCat + invType + invadedArea_invasiveSp + nativeArea_invasiveSp + invadedArea_nativeSp + nativeArea_nativeSp+cwmCalc ~ valueType)
#View(c.df)
#str(c.df)

# Ammend measures to clarify the data quality of the unit-standardized values
c.df$percMeasCov<-c.df$nMeasCov / c.df$nSp * 100 #calc perc cover measured
c.df$perc1spCov<-c.df$n1spCov / c.df$nSp * 100 #calc perc 1 sp measured
c.df$percOrigTr<-c.df$nOrigTr / c.df$nSp * 100 #calc perc orig trait data
c.df$percTryGS<-c.df$nTryGS / c.df$nSp * 100 #calc perc species trait data
#check out the distribution of data quality
ggplot(c.df, aes(x=percMeasCov)) + geom_histogram() #0, >0 and <100, 100
ggplot(c.df, aes(x=perc1spCov)) + geom_histogram() #0, >0 and <100, 100
ggplot(c.df, aes(x=percOrigTr)) + geom_histogram() #0, >0 and <100, 100
ggplot(c.df, aes(x=percTryGS)) + geom_histogram() #0, >0 and <100, 100
#bin it function
BinIt<-function(data, newCol, origCol){
  data[,newCol]<-rep(NA, dim(data)[1])
  data[data[,origCol] == 0 & !is.na(data[,origCol]), newCol] <- 'None'
  data[data[,origCol] >0 & data[,origCol] <100 & !is.na(data[,origCol]), newCol] <- 'Mid'
  data[data[,origCol] == 100 & !is.na(data[,origCol]), newCol] <- 'All'
  
  return(data)
}
datafr<-c.df
cats<-c('MeasCov','1spCov','OrigTr','TryGS')
newCol.vec<-paste('bin',cats, sep='')
origCol.vec<-paste('perc',cats, sep='')
datafr.list<-list()
for(i in 1:length(cats)){
  datafr<-BinIt(data=datafr, newCol=newCol.vec[i], origCol=origCol.vec[i])
}
#all bins
datafr$qualityBins<-apply(datafr[,colnames(datafr) %in% newCol.vec], 1, function(x) paste(x, collapse="_"))
#cover bins
vec1<-paste('Measured=',datafr$binMeasCov, sep="")
vec2<-paste('1sp=',datafr$bin1spCov, sep="")
datafr$coverBins<-paste(vec1, vec2, sep=", ")
#trait bins
vec1<-paste('Original=',datafr$binOrigTr, sep="")
vec2<-paste('SpeciesLevel=',datafr$binTryGS, sep="")
datafr$traitBins<-paste(vec1, vec2, sep=", ")
c.df2<-datafr
#colnames(c.df2)

# Remove unnecessary columns and reorganize
df1<-c.df2[,c('obsID','traitCat','invType',
              'invadedArea_invasiveSp','nativeArea_invasiveSp','invadedArea_nativeSp','nativeArea_nativeSp',
              'qualityBins','coverBins','traitBins',
              'mean')]
#str(df1)
df1$qualityBins<- as.factor(df1$qualityBins)
df1$coverBins<- as.factor(df1$coverBins)
df1$traitBins<- as.factor(df1$traitBins)

# Plot histograms for the standardized measure values
pHist<-ggplot(df1, aes(x=mean, fill=qualityBins)) + 
  facet_wrap(~traitCat, scales='free') + 
  geom_histogram()
#pHist

# Log-transform the means
#calculate how much needs to be added to a vector if its min is less than 0 (can't take the log of a negative value)
transform.c <- ddply(df1, ~traitCat, summarise, 
                     min=min(mean, na.rm=T))
transform.c 
#add columns of log-transformed data
df1$logTrans<-log(df1$mean)

pLog<-ggplot(df1, aes(x=logTrans, fill=qualityBins)) + 
  facet_wrap(~traitCat, scales='free') + 
  geom_histogram()
#pLog

# Pretty-fy unit-standardized data that has been transformed 
colnames(df1)[colnames(df1)=='qualityBins']<-'qualityForAll'
colnames(df1)[colnames(df1)=='coverBins']<-'qualityForCover'
colnames(df1)[colnames(df1)=='traitBins']<-'qualityForTraits'
colnames(df1)[colnames(df1)=='mean']<-'mean_cwm'
colnames(df1)[colnames(df1)=='logTrans']<-'mean_cwm_lnTF'
cwm<-df1


