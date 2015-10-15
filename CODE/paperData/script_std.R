#paperData_stdUnits/script_std.R
#Standardize values by the most common unit


### Load Fxns and Libraries ####################################################
library(plyr) #for ddply
library(doBy) # for orderBy
source('rmdCode/paperData/fxn_ConvertVar.R') # for FixVarTypes.Row()
source('rmdCode/paperData/fxn_FixUnits.R') # for FixUnits()




### Standardize Cover ####################################################

#1. Calucate covUnitList (also in MIIN_paperData.Rmd)
summ.covUnit <- ddply(cover,~covCat+covUnit,summarise, numMeas = length(obsID),numObs=length(unique(obsID)), numPapers=length(unique(paperID)))
COVCAT<-unique(summ.covUnit$covCat)
covUnitList<-list()
i<-0
for(i in 1:length(COVCAT)){
  subdf<-summ.covUnit[summ.covUnit$covCat==COVCAT[i],]
  covUnitList[[as.character(COVCAT[i])]]<-orderBy(~-numMeas, subdf)
}
#covUnitList

#2. Make sure that the data is coded right (WHAT DOES THAT MEAN?)
#str(covUnitList)
#str(cover)
#unique(cover$covUnit) #no NAs

#3. Assign a common unit and create index for each Cat
source('rmdCode/paperData/script_commUnit_cov.R') #TASK= Assign a common unit for each Cat; NEEDS= covUnitList; MAKES= c.unitIndex
#c.unitIndex
source('rmdCode/paperData/script_convertOps_cov.R') #TASK= Create an index for how to convert to common unit; NEEDS= c.unitIndex; MAKES= index.list
colnames(unitIndex)[1]<-'covCat'
#unitIndex

#4. Convert values to assigned common unit
#unitIndex
cover.new<-FixUnits(df=cover, 
                       Catcol='covCat', 
                       meancol=c('covInvMean','covNatMean'),
                       varcol=c('covInvVar_VAR','covNatVar_VAR'),
                       unitcol='covUnit',
                       unitIndex=unitIndex, 
                       invnat=TRUE)
#View(cover.new)
cover<-cover.new




### Standardize Traits ####################################################

#1. Calucate traitUnitList (also in MIIN_paperData.Rmd)
summ.traitUnit <- ddply(traits,~traitCat+traitUnit,summarise, 
                        numMeas = length(obsID),
                        numObs=length(unique(obsID)), 
                        numPapers=length(unique(paperID)))
#summ.traitUnit
TRAITCAT<-unique(summ.traitUnit$traitCat)
traitUnitList<-list()
i<-0
for(i in 1:length(TRAITCAT)){
  subdf<-summ.traitUnit[summ.traitUnit$traitCat==TRAITCAT[i],]
  traitUnitList[[as.character(TRAITCAT[i])]]<-orderBy(~-numMeas, subdf)
}
#traitUnitList

#2. Make sure that the data is coded right
#str(traitUnitList)
#str(traits)
# Problem: Need to get rid of the 'NA' under traits$measUnit
# Solution: Updated the raw data so that even C:N values had units
#unique(traits$traitUnit)
#unique(traits[is.na(traits$traitUnit),'traitCat']) #none

#3. Assign a common unit and create index for each Cat
source('rmdCode/paperData/script_commUnit_traits.R') # TASK= Assign a common unit for each Cat; NEEDS= covUnitList; MAKES= c.unitIndex
#c.unitIndex
source('rmdCode/paperData/script_convertOps_traits.R') # TASK= Create an index for how to convert to common unit; NEEDS= c.unitIndex; MAKES= index.list
colnames(unitIndex)[1]<-'traitCat'
#unitIndex

#4. Convert values to assigned common unit
#unitIndex
traits.new<-FixUnits(df=traits, 
                     Catcol='traitCat', 
                     meancol='traitMean',
                     varcol='traitVar_VAR',
                     unitcol='traitUnit',
                     unitIndex=unitIndex, 
                     invnat=FALSE)
#warning messages when there was no way to convert the unit and NA was inserted
#View(traits.new)
traits<-traits.new



### Standardize Measures ####################################################

#1. Calucate measUnitList (also in MIIN_paperData.Rmd)
summ.measUnit <- ddply(measures,~measCat+measUnit,summarise, 
                       numMeas = length(obsID),
                       numObs=length(unique(obsID)), 
                       numPapers=length(unique(paperID)))
#summ.measUnit
MEASCAT<-unique(summ.measUnit$measCat)
measUnitList<-list()
i<-0
for(i in 1:length(MEASCAT)){
  subdf<-summ.measUnit[summ.measUnit$measCat==MEASCAT[i],]
  measUnitList[[as.character(MEASCAT[i])]]<-orderBy(~-numMeas, subdf)
}
#measUnitList

#2. Make sure that the data is coded right
#str(measUnitList)
#str(measures)
# Problem: Need to get rid of the 'NA' under measures$measUnit
# Solution: Updated the raw data so that even C:N values had units
#unique(measures$measUnit)
#unique(measures[is.na(measures$measUnit),'measCat']) #none

#3. Assign a common unit and index to convert units to the common unit
source('rmdCode/paperData/script_commUnit_meas.R') # TASK= Assign a common unit for each measCat; NEEDS= measUnitList; MAKES= c.unitIndex
#c.unitIndex
source('rmdCode/paperData/script_convertOps_meas.R') #TASK= Assing an index to convert units to the common unit; NEEDS= c.unitIndex; MAKES= index.list
colnames(unitIndex)[1]<-'measCat'
#unitIndex

#4. Convert values to assigned common unit
measures.new<-FixUnits(df=measures, 
                       Catcol='measCat', 
                       meancol=c('measInvMean','measNatMean'),
                       varcol=c('measInvVar_VAR','measNatVar_VAR'),
                       unitcol='measUnit',
                       unitIndex=unitIndex, 
                       invnat=TRUE)
#View(measures.new)
#sub<-subset(measures.new, measCat == 'nh' & stdmeanInv > 1000) #fixed conversions so that I now don't get super high/incorrect values
measures<-measures.new





