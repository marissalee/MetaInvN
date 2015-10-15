#paperData_synthesis/script_convertOps_try.R
#Assign operation names that are needed to convert values from one unit to another


### Load functions ####################################################
source('rmdCode/paperData/fxn_makeIndex.R')
library(plyr) # for ldply

### Construct an index for each covCat that hold details on how to convert units ####################################################
#Remember: C:N values need to be in molC/molN; gC/gN * (14.0067/12.0107) -> molC/molN
# Make a list of charVecs to convert each CAT from curr unit to common unit
#c.unitIndex
#unitList

#convert to molC/molN
ctCN<-c('g/g * (14.0067/12.0107) ->molC/molN')

#convert to %
ctPerc<-c('mg/g * (100/1000) ->%')

#unique(c.unitIndex$commonUnit)
convertToList<-list(ctCN, ctPerc)
names(convertToList)<-unique(c.unitIndex$commonUnit)
#convertToList


# Go through each CAT and update stdType and stdOperation columns
index.list<-list()
CAT<-unique(c.unitIndex$Cat)
i<-0
for(i in 1:length(CAT)){
  c.unit<-c.unitIndex[c.unitIndex$Cat==CAT[i],'commonUnit'] #the common unit
  charVec<-convertToList[names(convertToList)==c.unit][[1]] #the character vector
  index.list[[as.character(CAT[i])]]<-MakeIndex(CAT=CAT[i], c.unit=c.unit, charVec=charVec) #make a pretty index and add it to the list
}
#index.list


# Turn the list into a dataframe
unitIndex<-ldply(index.list, data.frame)[,-1] #turn index into a dataframe
#View(unitIndex)





