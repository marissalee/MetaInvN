#paperData_stdUnits/script_convertOps_traits.R
#Assign operation names that are needed to convert values from one unit to another


### Load functions ####################################################
source('CODE/paperData/fxn_makeIndex.R') #for MakeIndex()
library(plyr) # for ldply




### Construct an index for each measCat that hold details on how to convert units ####################################################
#Remember: C:N values need to be in molC/molN; gC/gN * (14.0067/12.0107) -> molC/molN
# Make a list of charVecs to convert each CAT from curr unit to common unit
c.unitIndex
traitUnitList[[4]]

#convert to molC/molN; same as for 'measures'
ctCN<-c('% * (14.0067/12.0107) ->molC/molN',
        '%C/%N * (14.0067/12.0107) ->molC/molN',
        'gC/gN * (14.0067/12.0107) ->molC/molN')

#convert to %; same as for 'measures'
ctPerc<-c('mg/g * (100/1000) ->%',
          'g/g * (100/1) ->%',
          'g/kg * (100*1000) ->%',
          'ug/g * (100/1000000) ->%',
          'ug/mg * (100*1000/1000000) ->%')

#unique(c.unitIndex$commonUnit)
convertToList<-list(ctCN, ctPerc)
names(convertToList)<-unique(c.unitIndex$commonUnit)
#convertToList

# Go through each CAT and update stdType and stdOperation columns
index.list<-list()
CAT<-unique(c.unitIndex$traitCat)
i<-0
for(i in 1:length(CAT)){
  c.unit<-c.unitIndex[c.unitIndex$traitCat==CAT[i],'commonUnit'] #the common unit
  charVec<-convertToList[names(convertToList)==c.unit][[1]] #the character vector
  index.list[[as.character(CAT[i])]]<-MakeIndex(CAT=CAT[i], c.unit=c.unit, charVec=charVec) #make a pretty index and add it to the list
}
index.list

# Turn the list into a dataframe
unitIndex<-ldply(index.list, data.frame)[,-1] #turn index into a dataframe
#unitIndex


