#tryData/script_traitKey.R
#Make a trait data key

### Load Libraries ####################################################
library(plyr)




### Pull out the unique trait IDs and names ####################################################

dataT<-read.table('synthesizedData/tryData/tryDataT.txt', sep='\t', header=T) 

#list of traits in dataT
traitids<-unique(dataT$TraitID) # TraitID nums
#traitids
traitnames<-as.character(unique(dataT$TraitName)) #TraitName
#traitnames
#length(traitids)

#list of subtraits (dataID) in dataT
dataids<-unique(dataT$DataID)
#dataids
datanames<-as.character(unique(dataT$DataName)) 
#datanames
#length(dataids)




### Pull out the unique data IDs and names ####################################################

#make an index of subtraits (dataID) that fall under each traitID
id.list<-list()
name.list<-list()
i<-0
for (i in 1:length(traitids)){
  temp<-dataT[dataT$TraitID==traitids[i],]
  id.list[[as.character(traitids[i])]]<-unique(temp$DataID)
  name.list[[as.character(traitnames[i])]]<-unique(temp$DataName)
}
#id.list 
#name.list
#ldply(name.list, length) #distribution of dataids per traitid

#reorganize these lists so that can be merged into one table
new.id.list<-list()
new.name.list<-list()
i<-0
for(i in 1:length(id.list)){
  dataIDs<-id.list[[i]]
  n<-length(dataIDs)
  traitID<-names(id.list[i])
  new.id.list[[i]]<-data.frame(traitID = rep(traitID,n), dataID = dataIDs)
  
  dataNames<-name.list[[i]]
  traitName<-names(name.list[i])
  new.name.list[[i]]<-data.frame(traitName = rep(traitName,n), dataName = dataNames)  
}
new.id.tab<-ldply(new.id.list)
new.name.tab<-ldply(new.name.list)

tdIndex<-data.frame(traitID=new.id.tab$traitID, traitName=new.name.tab$traitName, 
           dataID=new.id.tab$dataID, dataName=new.name.tab$dataName)
#tdIndex




### Make a list for the corresponding Units ####################################################

#dataids #unique list of dataids
ids.units<-character(0)
i<-0
for (i in 1:length(dataids)){
  temp<-dataT[dataT$DataID==dataids[i],]
  row<-unique(temp$Unit_1_UnitName)
  ids.units<-rbind(ids.units,as.character(row))
} # all 'rows' were 1 element long, meaning that only 1 unit type is used for each DataID
#ids.units

#link units to their corresponding dataID
unittab<-data.frame(dataID=dataids, unit=ids.units)

#ammend the tdIndex with units by indexing dataID in the unittab

tdIndex$unit<-rep(NA,dim(tdIndex)[1])
i<-0
for(i in 1:length(dataids)){
  approp.unit<-as.character(unittab[unittab$dataID==dataids[[i]],'unit'])
  tdIndex[tdIndex$dataID==dataids[[i]],'unit']<-approp.unit
}
#head(tdIndex)




### Make a list of the corresponding paper traits of interest ####################################################
# cn, percN, littercn, litterpercN

#subset the DataNames with 'nitrogen'
dataName_nitrogen<-tdIndex[grep("nitrogen", tdIndex$dataName),c('dataName','unit')]

#then, remove DataNames with the following attributes
#dataName_nitrogen$dataName
rows<-c(grep("Root",dataName_nitrogen$dataName),
        grep("root",dataName_nitrogen$dataName), 
        grep("Stem",dataName_nitrogen$dataName), 
        grep("Bark",dataName_nitrogen$dataName),
        grep("Whole",dataName_nitrogen$dataName),
        grep("area",dataName_nitrogen$dataName),
        grep("Total", dataName_nitrogen$dataName),
        grep("organic", dataName_nitrogen$dataName))
dataName_n1<-dataName_nitrogen[-rows,]
#dataName_n1

#identify traitsOfInterest
tdIndex$traitOI<-rep(NA, dim(tdIndex)[1])
#tdIndex[tdIndex$dataName %in% dataName_n1$dataName,]
tdIndex[tdIndex$dataName %in% dataName_n1$dataName, 'traitOI'] <- traitOI<-c('percN',
                                                                             'cn',
                                                                             'litterpercN',
                                                                             NA,
                                                                             'littercn')



