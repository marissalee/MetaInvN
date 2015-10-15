#paperData_stdUnits/script_convertOps_cov.R
#Assign operation names that are needed to convert values from one unit to another


### Load functions ####################################################
source('rmdCode/paperData/fxn_makeIndex.R')


### Construct an index for each covCat that hold details on how to convert units ####################################################
c.unitIndex
covUnitList

# Make a list of charVecs that match each CAT
cv1<- c('kg/m2 * (1000/1) ->g/m2')
cv2<- c('')
cv3<- c('') 

cv.list<-list(cv1,cv2,cv3)
CAT<-unique(c.unitIndex$covCat)
names(cv.list)<-CAT

# Go through each CAT and update stdType and stdOperation columns
index.list<-list()
i<-0
for(i in 1:length(CAT)){
  c.unit<-c.unitIndex[c.unitIndex$covCat==CAT[i],'commonUnit'] #the common unit
  charVec<-cv.list[[i]] #the character vector
  index.list[[as.character(CAT[i])]]<-MakeIndex(CAT=CAT[i], c.unit=c.unit, charVec=charVec) #make a pretty index and add it to the list
}
#index.list


# Turn the list into a dataframe
unitIndex<-ldply(index.list, data.frame)[,-1] #turn index into a dataframe
#View(unitIndex)


