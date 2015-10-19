#CODE/paperData/script_load_checkCover.R
#Closer look at the structure of 'cover'

## PART I. Overview ####################################################
##A. Make sure each observations has a cover measurement, specifically a sp_plantcov measurement
NoCov1<-!unique(cover$obsID) %in% unique(observations$obsID) #Does each observation have a cover entry at all?
unique(cover$obsID)[NoCov1=='TRUE'] # should be 0, if not then these obsIDs in the cover file do not have a correponding obsID in the observations file
NoCov2<-!unique(observations$obsID) %in% unique(cover$obsID)
unique(observations$obsID)[NoCov2=='TRUE'] # should be 0, if not then these obsIDs in the observations file do not have a corresponding obsID in the cover file
cover.sub<-cover[which(cover$covCat=='sp_plantcov'),] # Subset cover data so that we are only looking at sp_plantcover
NoCov1<-!unique(cover.sub$obsID) %in% unique(observations$obsID) #Does each observation have a cover entry that is sp_plantcover?
unique(cover.sub$obsID)[NoCov1=='TRUE'] # should be 0, if not thenthese obsIDs in the cover file do not have a correponding obsID in the observations file
NoCov2<-!unique(observations$obsID) %in% unique(cover.sub$obsID)
unique(observations$obsID)[NoCov2=='TRUE'] # should be 0, if not thenthese obsIDs in the observations file do not have a corresponding obsID in the cover file

##B. Subset cover data by the number of species included in the sp_plantcov
cover.sub.1sp<-cover.sub[cover.sub$covNumSpp == '1 species',]
cover.sub.Xsp<-cover.sub[cover.sub$covNumSpp == '>1 species',]

#i. Make sure that there is only 1 covSpEntryID listed in rows where there is only supposed to be 1 species
cover.sub.1sp$numsp<-ldply(strsplit(as.character(cover.sub.1sp$covSpEntryID), ','), length)[,1] #make a column that holds the number of species
sum(cover.sub.1sp$numsp != 1) # should be 0, if not then there is an entry with >1 spEntryID (mislabeled)
cover.sub.1sp[cover.sub.1sp$numsp!=1,] # none. good.

#ii. Make sure there is >1 covSpEntryID listed in rows where there is supposed to be >1 species
tmp<-strsplit(as.character(cover.sub.Xsp$covSpEntryID), ',') #split the list of sp#s in covSpEntryID
cover.sub.Xsp$numsp<-ldply(strsplit(as.character(cover.sub.Xsp$covSpEntryID), ','), length)[,1] #make a column that holds the number of species
sum(cover.sub.Xsp$numsp == 1) # this should be 0, if not then there is an entry with only 1 spEntryID although it is labeled as an entry with >1sp
cover.sub.Xsp[cover.sub.Xsp$numsp==1,'covDescript'] # its ok; although there is only 1 spEntryID, it is supposed to be labelled as >1sp

##C. For cover data where there is more than 1 species, expand the table to fit each species in its own row
splist<-strsplit(as.character(cover.sub.Xsp$covSpEntryID), ',') # strsplit the covSpEntryID string and count the number of species (numsp)
library(reshape)
expanded<-untable(cover.sub.Xsp, num=cover.sub.Xsp[,'numsp']) #need library(reshape), use the column with the number of species to expand the table
expanded$covSpEntryID<-as.numeric(unlist(splist)) #update the covSpEntryID that is specific to each row in this expanded table
expanded$spID<-paste(expanded$obsID, expanded$covSpEntryID, sep='.') #identify the spIDs by pasting the obsID and the SpEntryID listed in 'cover'
tmp<-ddply(expanded, ~spID, summarise, n = length(spID)) #check to make sure you created unique spIDs while expanding the 'covSpEntryID' column
tmp[tmp$n >1,] # should be 0, if not then duplicate spIDs were created and you need to figure out why

##D. Fill the created spIDs in the expanded cover df by referencing species names in 'species' by spID

#i. There are spIDs that were created from spEntryID == NA... need to exclude these first
SPID<-unique(expanded$spID)
numIDd<-SPID %in% species$spID # identify the position of spIDs referenced in the cover file based on where they are in the species file
SPID[!numIDd] # these are the spIDs that are not present in the species file
removeRows<-expanded$spID %in% SPID[!numIDd]
expanded1<-expanded[!removeRows,]

#ii. Fill spIDs in expanded subtable
source('CODE/cwm/fxn_idHelpers.R')
cover.Xsp.expanded<-Add.spID.Names(expanded1) # now this should work




## PART II. Check that 1sp data match 'species' spIDs and species names ####################################################
spIndex<-species[,c('spID','spName')]

##A. 'cover'
covIndex<-ddply(cover, ~spID, summarise, spnam = paste(unique(covDescript),collapse='___'))
dim(spIndex[spIndex$spID %in% covIndex$spID,]); dim(covIndex[covIndex$spID %in% spIndex$spID,]); dim(covIndex) #link up the dimensions
covIndex[!covIndex$spID %in% spIndex$spID,] # the covIndex has spIDs that are not in the species dataframe because these are ones where spID = NA
spI<-orderBy(~spID, spIndex[spIndex$spID %in% covIndex$spID,])
covI<-orderBy(~spID, covIndex[covIndex$spID %in% spIndex$spID,])

#i. Same unique spIDs? 
identical(spI[,'spID'], covI[,'spID']) #if TRUE, then yes, unique spIDs are the same

#ii. Same species names for each unique spID?
sum(!as.character(spI[,'spName']) == as.character(covI[,'spnam'])) # should be 0, if not then species names are not identical
nonMatchingSPID<-data.frame(species=spI[!as.character(spI[,'spName']) == as.character(covI[,'spnam']),], 
                            cover=covI[!as.character(spI[,'spName']) == as.character(covI[,'spnam']),])
nonMatchingSPID #I'm okay with all of these ...
# #Problem: The spIDs in 'cover' does not refer to exactly the same species names in 'species'
# #Found this problem with... the code to check 'cover' and 'species' spIDs above.
# #Solution: ....
# #If a typo, then fix it in the raw data. --- all of these were typos in covSpEntryID or the covDescript
# #If not a typo, then decide if you want to accept species-level trait data for the species name listed in 'species'.  
# #If ok, let it be but make sure to use 'species' spID as the key
# #If not ok, change the spID in 'cover' to NA 




## PART III. Check that Xsp data match 'species' spIDs and species names ####################################################

##A. Check that unlisted 'cover' species match 'species' spIDs and species names
covIndex<-ddply(cover.Xsp.expanded, ~spID, summarise, covDescript = unique(covDescript), spnam = GenusSpecies)
spIndex<-species[,c('spID','spName')]
sum(!covIndex$spID %in% spIndex$spID) #if 0, then all covIndex spIDs are listed in species
spI<-orderBy(~spID, spIndex[spIndex$spID %in% covIndex$spID,])
covI<-orderBy(~spID, covIndex[covIndex$spID %in% spIndex$spID,])

#i. Same unique spIDs? 
sum(!spI[,'spID'] ==  covI[,'spID']) # if 0, then spIDs match

#ii. Same species names for each unique spID?
sum(!spI[,'spName'] == covI[,'spnam'], na.rm=T) # this should be 0.  If not, that measures that the species names are not identical

