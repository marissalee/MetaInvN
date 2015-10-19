#script_TRYstdUnits.R
#Convert TRY data to the common unit for each trait type

### Load Fxns and Libraries ####################################################
source('CODE/cwm/fxn_idHelpers.R') # for AddTraitOI
library(plyr) #for ddply
library(doBy) # for orderBy


#1. Subset tryDataT by the traits of interest (traitID) and add indexing column
traitIDs<-tryData_traitKey[grepl("N", tryData_traitKey$traitName),'traitID'] #pull out the traitIDs where the name has nitrogen in it (N)
tryDataT.subt<-tryDataT[tryDataT$TraitID %in% traitIDs,]
trydf<-AddTraitOI(df=tryDataT.subt, template=tryData_traitKey) #need synthesis/fxn_idHelpers.R
trydf1<-trydf[!is.na(trydf$traitOI),] # remove rows with a traitOI == NA
tmp<-ddply(trydf1, ~AccSpeciesName+traitOI, summarise, nDataIDs = length(unique(dataName))) #check the number of dataIDs per traitOI
sum(tmp$nDataIDs > 1) #this should be 0. If not, that means that there is more than 1 dataID entry per traitOI for some species

#2. Calucate unitList
summ.Unit <- ddply(trydf1,~traitOI+unit,summarise, numMeas = length(traitOI))
CAT<-unique(summ.Unit$traitOI)
unitList<-list()
i<-0
for(i in 1:length(CAT)){
  subdf<-summ.Unit[summ.Unit$traitOI==CAT[i],]
  unitList[[as.character(CAT[i])]]<-orderBy(~-numMeas, subdf)
}
unitList

#2. Make sure that the data is coded right
#str(unitList)
#str(trydf1)
#unique(trydf1$unit) #no NAs
unique(trydf1$RelUncertainty) #there is no VAR data

#3. Assign a common unit and create index for each Cat
source('CODE/cwm/script_commUnit_try.R') #TASK= Assign a common unit for each Cat; NEEDS= unitList; MAKES= c.unitIndex
source('CODE/cwm/script_convertOps_try.R') #TASK= Create an index for how to convert to common unit; NEEDS= c.unitIndex; MAKES= index.list
colnames(unitIndex)[1]<-'traitOI'

#4. Convert values to assigned common unit
#trydf1
#'traitOI'
#'StdValue'
#'unit'
#unitIndex

#This is modified from FixUnits()...
# Create extra columns in the df to hold the new values
emptylength<-dim(trydf1)[1]
trydf1$stdmean<-rep(NA,emptylength)
trydf1$stdvar<-rep(NA,emptylength)
trydf1$stdunit<-rep(NA,emptylength)
#View(trydf1)

TRAITS<-unique(trydf1$traitOI)
i<-0
for(i in 1:length(TRAITS)){
  
  #Trait value lookup
  rowtry<-trydf1$traitOI == TRAITS[i]
  curr.unit<-trydf1[rowtry,'unit']
  curr.means<-trydf1[rowtry,'StdValue']
  curr.vars<-NA
  
  # Unit Index lookup
  rowindex<-unitIndex$traitOI == TRAITS[i]
  c.unit<-unitIndex[rowindex,'c.unit']
  conv.unit<-unitIndex[rowindex,'conv.unit']
  needed.multiplier<-as.character(unitIndex[rowindex,'multiplier'])
  params<-as.numeric(unlist(strsplit(needed.multiplier, '/', fixed=T))) #if there is division...identify the numerator and denominator
  conv.factor<-params[1]/params[2] #divide the numerator by the denominator to identify the value to multiply by

  # Multiply mean by the conv.factor to convert the mean into c.unit and put into appropriate df cols
  trydf1[rowtry,'stdmean']<-curr.means * conv.factor
  trydf1[rowtry,'stdvar']<-curr.vars * conv.factor^2 #remember that Var(aX) = a^2 * Var(X)
  trydf1[rowtry,'stdunit']<-as.character(c.unit)
}
#View(trydf1)

tryDataT.cu<-trydf1
