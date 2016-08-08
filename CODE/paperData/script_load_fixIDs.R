#CODE/paperData/script_load_fixIDs.R
#Fix ID keys (obsID, AggNum, paperID, aggID, spID 

### A. Make keys 'obsID' and 'xAggNum' numeric in each dataset ####################################################
#Currently, obsID and xAggNum columns in raw excel files are coded as factors

#i. obsID
observations$obsID<-as.numeric(as.character(observations$obsID))
measures$obsID<-as.numeric(as.character(measures$obsID))
measAgg$obsID<-as.numeric(as.character(measAgg$obsID))
cover$obsID<-as.numeric(as.character(cover$obsID))
covAgg$obsID<-as.numeric(as.character(covAgg$obsID))
traits$obsID<-as.numeric(as.character(traits$obsID))
traitAgg$obsID<-as.numeric(as.character(traitAgg$obsID))
#check that there are no duplicate obsIDs 
length(unique(observations$obsID))
dim(observations)[1] #is there a duplicate? No.

#ii. xAggNum
measures$measAggNum<-as.numeric(as.character(measures$measAggNum))
measAgg$measAggNum<-as.numeric(as.character(measAgg$measAggNum))
cover$covAggNum<-as.numeric(as.character(cover$covAggNum))
covAgg$covAggNum<-as.numeric(as.character(covAgg$covAggNum))
traits$traitAggNum<-as.numeric(as.character(traits$traitAggNum))
traitAgg$traitAggNum<-as.numeric(as.character(traitAgg$traitAggNum))

### B. Add ID columns ####################################################

#i. paperID
observations$paperID<-as.numeric(as.character(observations$paperID))
measures$paperID<-as.integer(measures$obsID)
cover$paperID<-as.integer(cover$obsID)
traits$paperID<-as.integer(traits$obsID)
species$paperID<-as.integer(species$obsID)

#ii. aggID: combine obsID and xAggNum
measures$aggID<-paste(measures$obsID, measures$measAggNum, sep='.')
measAgg$aggID<-paste(measAgg$obsID, measAgg$measAggNum, sep='.')
cover$aggID<-paste(cover$obsID, cover$covAggNum, sep='.')
covAgg$aggID<-paste(covAgg$obsID, covAgg$covAggNum, sep='.')
traits$aggID<-paste(traits$obsID, traits$traitAggNum, sep='.')
traitAgg$aggID<-paste(traitAgg$obsID, traitAgg$traitAggNum, sep='.')
#check for missing aggIDs in measures
usedAgg<-unique(measAgg$aggID) %in% unique(measures$aggID)
allAgg<-unique(measures$aggID) %in% unique(measAgg$aggID)
length(usedAgg); sum(usedAgg); sum(allAgg) #should all be the same value
# #which aggIDs are in the measAgg table, but not in the measures table?
# u.measAgg<-unique(measAgg$aggID)
# u.measAgg[usedAgg == FALSE]
#check for missing aggIDs in cover
usedAgg<-unique(covAgg$aggID) %in% unique(cover$aggID)
allAgg<-unique(cover$aggID) %in% unique(covAgg$aggID)
length(usedAgg); sum(usedAgg); sum(allAgg) #should all be the same value
#check for missing aggIDs in traits
usedAgg<-unique(traitAgg$aggID) %in% unique(traits$aggID)
allAgg<-unique(traits$aggID) %in% unique(traitAgg$aggID)
length(usedAgg); sum(usedAgg); sum(allAgg) #should all be the same value
# #which aggIDs are in the traitAgg table, but not in the traits table?
# u.traitAgg<-unique(traitAgg$aggID)
# u.traitAgg[usedAgg == FALSE]


#iii. spID: combine obsID and spEntryID

#First, check to see if there will be duplicate spIDs
require(plyr)
tmp<-ddply(species, ~obsID+spEntryID, summarise, n = length(obsID))
sum(tmp$n > 1) # this should be 0.  If not, there will be duplicate spIDs
#morethan1<-which(tmp$n > 1)
#tmp[morethan1,]
#species[species$obsID==704.02 & species$spEntryID==2,]
#Add to species
species$spID<-paste(species$obsID, species$spEntryID, sep='.')

#Add to traits
traits$spID<-paste(traits$obsID, traits$traitSpEntryID, sep='.')
traits[grepl(',', traits$spID),'spID']<-NA # if the traitSpEntryID listed more than 1 spEntryID, then just replace the spID with NA, since it is not species-specific
traits[grepl('NA', traits$spID),'spID']<-NA # if the traitSpEntryID includes 'NA', then just replace the spID with NA, since it is not species-specific
tmp<-ddply(traits, ~traitCat+spID, summarise, n = length(spID))
sum(tmp$n > 1) # this should be 0.  If not, there will be duplicate spIDs
tmp[tmp$n > 1,] # this is ok, because it is NA

#Add to cover
cover$spID<-paste(cover$obsID, cover$covSpEntryID, sep='.')
cover[grepl(',',cover$spID),'spID']<-NA # if the covSpEntryID listed more than 1 spEntryID, then just replace the spID with NA, since it is not species-specific
cover[grepl('NA', cover$spID),'spID']<-NA # if the covSpEntryID includes 'NA', then just replace the spID with NA, since it is not species-specific
tmp<-ddply(cover, ~covCat+spID, summarise, n = length(spID))
sum(tmp$n > 1) # this should be 0.  If not, there will be duplicate spIDs
tmp[tmp$n > 1,] # this is ok, because it is NA
# # Problem: Duplicate spIDs were produced in 'cover'
# # Found this problem with... the code above
# # Solution: Look at each problematic spID
# tmp1<-tmp[tmp$n >1,'spID']
# tmp2<-tmp1[!is.na(tmp1)]
# tmp2
# i<-0
# for(i in 1:length(tmp2)){
#   print(cover[cover$spID %in% tmp2[i],])
# }


#vi. coverID: combine obsID and covEntryID
cover$coverID<-paste(cover$obsID, as.numeric(cover$covEntryID), sep='.')
length(unique(cover$coverID))
dim(cover)[1] #is there a duplicate? No.

#v. traitID: combine obsID and traitEntryID
traits$traitID<-paste(traits$obsID, as.numeric(traits$traitEntryID), sep='.')
length(unique(traits$traitID))
dim(traits)[1] #is there a duplicate? No.




