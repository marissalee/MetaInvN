#paperData_summstats/script_TFmeasStdUnit.R

### Load Fxns and Libraries ####################################################
library(ggplot2)
library(reshape2)

### Create a histogram for each measure, show values that were converted and/or aggregated
df<-measures
#colnames(df)


# Ammend measures to clarify the data quality of the values
df$measEntryID2<-paste(df$obsID, as.numeric(df$measEntryID), sep=".") #identify each row
df$AggYN<-rep("NoAgg", dim(df)[1]) #No aggregation
df[!is.na(df$measAggNum),'AggYN']<-"Agg" # Aggregation
df$UnitConvYN<-rep("NoConv", dim(df)[1]) # No unit conversion
df[df$measUnit != as.character(df$stdunit) & !is.na(df$stdunit),'UnitConvYN']<-"Conv" # Unit conversion
df$YN<-paste(df$AggYN, df$UnitConvYN, sep=".")




### Unit-standardized means ####################################

# Create and reshape the unit-standardized means so that inv and nat area is 1 factor column
df1<-df[,c('obsID','measEntryID2','measCat','YN',
           'stdmeanInv','stdmeanNat')] # unit-standardized means
m.df1<-melt(df1, id=c('obsID','measEntryID2','measCat','YN')) #melt
#add column to differentiate between inv and nat
m.df1$invType<-rep(NA,length(dim(m.df1)[1])) #add column to differentiate between inv and nat
m.df1[m.df1$variable=='stdmeanInv','invType']<-'inv'
m.df1[m.df1$variable=='stdmeanNat','invType']<-'nat'

# Log-transform the means
#calculate how much needs to be added to a vector if its min is less than 0 (can't take the log of a negative value)
tabTF <- ddply(m.df1, ~measCat, summarise, min=min(value, na.rm=T))
tabTF$constant<-rep(0,dim(tabTF)[1])
tabTF[tabTF$min <= 0,'constant']<-1
m.df2<-merge(m.df1, tabTF, by='measCat')
#add columns of log-transformed data
m.df2$logTrans<-log(m.df2$value + m.df2$constant)
#reorganize column order
meas<-m.df2[,c('obsID','measEntryID2','measCat','variable','invType','YN','constant','value','logTrans')]

# Plot the basic and log-transformed histograms for the standardized measure values
#subset by the variables that I want to look at now...
df.sub<-subset(meas, measCat %in% c("nh","no", "toti","ammonif", "nitrif","nminz","soilmoi","som","soilcn","ph"))
#change the order that the measCats are presented
df.sub <- transform(df.sub, measCat = factor(measCat, levels=c("nh", "no", "toti","ammonif", "nitrif","nminz","soilmoi","som","soilcn","ph")))
#plot
pHist_stdunits<-ggplot(df.sub, aes(x=value,fill=YN)) + 
  facet_wrap(~measCat, scales='free', nrow=4, ncol=3) + 
  geom_histogram()
pLog_stdunits<-ggplot(df.sub, aes(x=logTrans,fill=YN)) + 
  facet_wrap(~measCat, scales='free', nrow=4, ncol=3) + 
  geom_histogram()

# Pretty-fy unit-standardized data that has been transformed 
colnames(meas)[colnames(meas)=='YN']<-'qualityForMeas'
colnames(meas)[colnames(meas)=='value']<-'mean_stdunits'
colnames(meas)[colnames(meas)=='logTrans']<-'mean_stdunits_lnTF'
#create and reshape the unit-standardized vars so that inv and nat area is 1 factor column
df1<-df[,c('obsID','measEntryID2','measCat','stdunit','stdvarInv','stdvarNat')]
m.df1<-melt(df1, id=c('obsID','measEntryID2','measCat','stdunit')) #melt
#add column to differentiate between inv and nat
m.df1$invType<-rep(NA,length(dim(m.df1)[1])) #add column to differentiate between inv and nat
m.df1[m.df1$variable=='stdvarInv','invType']<-'inv'
m.df1[m.df1$variable=='stdvarNat','invType']<-'nat'
#merge the means and vars
tmp<-merge(meas, m.df1, by=c('obsID','measEntryID2','measCat','invType'))
colnames(tmp)[colnames(tmp)=='value']<-'var_stdunits'
meas_stdunits<-tmp[,!colnames(tmp) %in% c('variable.x', 'variable.y')]
#View(meas_stdunits)

# ### Effect size means #################################### -- do this later using metafor package
# 
# # Isolate the effect size means
# measES<-df[,c('obsID','measEntryID2','measCat','YN',
#            'd','lnR')] # unit-standardized means
# # Plot the basic and log-transformed histograms for the standardized measure values
# #subset by the variables that I want to look at now...
# 
# df.sub<-subset(measES, measCat %in% c("nh","no", "toti","ammonif", "nitrif","nminz","soilmoi","som","soilcn","ph"))
# #change the order that the measCats are presented
# df.sub <- transform(df.sub, measCat = factor(measCat, levels=c("nh", "no", "toti","ammonif", "nitrif","nminz","soilmoi","som","soilcn","ph")))
# #plot
# pHist_d<-ggplot(df.sub, aes(x=d,fill=YN)) + 
#   facet_wrap(~measCat, scales='free',  nrow=4, ncol=3) + 
#   geom_histogram()
# pHist_lnR<-ggplot(df.sub, aes(x=lnR, fill=YN)) + 
#   facet_wrap(~measCat, scales='free',  nrow=4, ncol=3) + 
#   geom_histogram()
# 
# # Pretty-fy effect size data
# tmp<-df[,c('obsID','measEntryID2','measCat','YN','d','lnR','vd','vlnR','n')] # unit-standardized means
# colnames(tmp)[colnames(tmp)=='d']<-'mean_d'
# colnames(tmp)[colnames(tmp)=='lnR']<-'mean_lnR'
# colnames(tmp)[colnames(tmp)=='vd']<-'var_d'
# colnames(tmp)[colnames(tmp)=='vlnR']<-'var_lnR'
# colnames(tmp)[colnames(tmp)=='YN']<-'qualityForMeas'
# meas_ES<-tmp
# #View(meas_ES)


