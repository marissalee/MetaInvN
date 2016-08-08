#paperData_stdUnits/script_convertOps_meas.R
#Assign operation names that are needed to convert values from one unit to another


### Load functions ####################################################
source('CODE/paperData/fxn_makeIndex.R') #for MakeIndex()
require(plyr) # for ldply




### Construct an index for each measCat that hold details on how to convert units ####################################################
#Remember: C:N values need to be in molC/molN; gC/gN * (14.0067/12.0107) -> molC/molN
# Make a list of charVecs to convert each CAT from curr unit to common unit
c.unitIndex
#measUnitList[[15]]

#convert to ppm
ctPPM<-c('ug/g * (1) ->ppm',
         'ug/kg * (1/1000) ->ppm',
         'mg/kg * (1) ->ppm',
         'mg/L * (1) ->ppm')

#convert to ppm/d
ctPPMD<-c('ug/g*hr * (24) ->ppm/d',
          'ug/g*d * (1) ->ppm/d',
          'mg/kg*d * (1) ->ppm/d',
          'umol/g*d * (14.0067/1000000) ->ppm/d',
          'mg/kg*10d * (1/10) ->ppm/d',
          'mg/kg*28d * (1/28) ->ppm/d',
          'ug/g*30d * (1/30) ->ppm/d',
          'ug/g*mo * (1/30) ->ppm/d',
          'ug/g*y * (1/365) ->ppm/d')

#convert to g/m2
ctGM2<-c('kg/m2 * (1/1000) ->g/m2',
         'kg/ha * (10/1) ->g/m2')

#convert to molC/molN
ctCN<-c('%C/%N * (14.0067/12.0107) ->molC/molN',
        'gC/gN * (14.0067/12.0107) ->molC/molN')

#convert to %
ctPerc<-c('mg/g * (100/1000) ->%',
          'g/g * (100/1) ->%',
          'g/kg * (100/1000) ->%',
          'ug/g * (100/1000000) ->%',
          'ppm * (100/1000000) ->%')

#convert to pH
ctPH<-c('')

unique(c.unitIndex$commonUnit)
convertToList<-list(ctPPMD, ctGM2, ctCN, ctPerc, ctPPM, ctPH)
names(convertToList)<-unique(c.unitIndex$commonUnit) 
#convertToList


# Go through each CAT and update stdType and stdOperation columns
index.list<-list()
CAT<-unique(c.unitIndex$measCat)
i<-0
for(i in 1:length(CAT)){
  c.unit<-c.unitIndex[c.unitIndex$measCat==CAT[i],'commonUnit'] #the common unit
  charVec<-convertToList[names(convertToList)==c.unit][[1]] #the character vector
  index.list[[as.character(CAT[i])]]<-MakeIndex(CAT=CAT[i], c.unit=c.unit, charVec=charVec) #make a pretty index and add it to the list
} 
#index.list


# Turn the list into a dataframe
unitIndex<-ldply(index.list, data.frame)[,-1] #turn index into a dataframe
#View(unitIndex)

