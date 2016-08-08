#CODE/paperData/script_load_checkTraits.R
#Closer look at the structure of 'traits' 

## PART I. Overview ####################################################
##A. Create a column to identify which 'traits' rows correspond to >1 spID
splist<-strsplit(as.character(traits$traitSpEntryID), ',') # strsplit the covSpEntryID string and count the number of species (numsp)
tmp<-ldply(llply(splist, length))
tmp.df<-data.frame(traitID=traits$traitID, NumSpID=tmp[,1])
traits[tmp.df$NumSpID >1,'traitSpEntryID']
traits$trNumSpp<-NA
traits[tmp.df$NumSpID == 1,'trNumSpp']<-'1 species'
traits[tmp.df$NumSpID >1,'trNumSpp']<-'>1 species'

##B. Select 'traits' where row includes >1 species for each traitCat and make a list of these dataframes
traits.sub.Xsp<-traits[traits$trNumSpp == '>1 species',]
CATS<-unique(traits.sub.Xsp$traitCat)
list.Xsp.TraitDfs<-list()
i<-0
for(i in 1:length(CATS)){
  list.Xsp.TraitDfs[[i]]<-traits.sub.Xsp[traits.sub.Xsp$traitCat == CATS[i],]
}
list.Xsp.TraitDfs

##C. For trait data where there is more than 1 species, expand the table to fit each species in its own row - keep this in a list of dataframes
list.Xsp.expanded.traitDfs<-list()
check.list<-list()
require(reshape)
i<-0
for(i in 1:length(CATS)){
  
  #select correct df from the list
  traits.sub.Xsp<-list.Xsp.TraitDfs[[i]]
  
  #expand it
  traits.sub.Xsp$numsp<-ldply(strsplit(as.character(traits.sub.Xsp$traitSpEntryID), ','), length)[,1] #make a column that holds the number of species
  splist<-strsplit(as.character(traits.sub.Xsp$traitSpEntryID), ',') # strsplit the traitSpEntryID string and count the number of species (numsp)
  expanded<-untable(traits.sub.Xsp, num=traits.sub.Xsp[,'numsp']) #need library(reshape), use the column with the number of species to expand the table
  expanded$traitSpEntryID<-as.numeric(unlist(splist)) #update the covSpEntryID that is specific to each row in this expanded table
  expanded$spID<-paste(expanded$obsID, expanded$traitSpEntryID, sep='.') #identify the spIDs by pasting the obsID and the SpEntryID listed in 'cover'
  tmp<-ddply(expanded, ~spID, summarise, n = length(spID)) #check to make sure you created unique spIDs while expanding the 'covSpEntryID' column
  check.list[[i]]<-tmp[tmp$n >1,] # should be 0, if not then duplicate spIDs were created and you need to figure out why
  
  #save it into a new df list
  list.Xsp.expanded.traitDfs[[i]]<-expanded
}
check.list # should be 0, if not then duplicate spIDs were created and you need to figure out why
#list.Xsp.expanded.traitDfs

##D. Fill the created spIDs in the expanded cover df by referencing species names in 'species' by spID

#i. There are spIDs that were created from spEntryID == NA... need to exclude these first
check.list<-list()
i<-0
for(i in 1:length(CATS)){
  
  #select correct df from the list
  expanded<-list.Xsp.expanded.traitDfs[[i]]
  
  #make sure the spID that was created for the expanded file exists in the species df
  SPID<-unique(expanded$spID)
  numIDd<-SPID %in% species$spID # identify the position of spIDs referenced in the cover file based on where they are in the species file
  check.list[[i]]<-SPID[!numIDd] # these are the spIDs that are not present in the species file
}
check.list # should be 0, if not then there are spIDs in the expanded trait df that are not present in the species file... will need to remove those rows

#ii. Fill spIDs in expanded subtable
traits.Xsp.expandedList<-list()
i<-0
for(i in 1:length(CATS)){
  #select correct df from the list
  expanded<-list.Xsp.expanded.traitDfs[[i]]
  
  #add sp names
  traits.sub.expanded<-Add.spID.Names(expanded) # now this should work
  
  #save it into a new df list
  traits.Xsp.expandedList[[i]]<-traits.sub.expanded
}
#traits.Xsp.expandedList




## PART II. Check that 1sp data match 'species' spIDs and species names ####################################################
spIndex<-species[,c('spID','spName')]

##A. 'traits'
trIndex<-ddply(traits, ~spID, summarise, spnam = paste(unique(traitSpName),collapse='___')) # organize 'traits' by unique spIDs
spIndex<-species[,c('spID','spName')]
dim(spIndex[spIndex$spID %in% trIndex$spID,]); dim(trIndex[trIndex$spID %in% spIndex$spID,]); dim(trIndex) #link up the dimensions
trIndex[!trIndex$spID %in% spIndex$spID,] # the trIndex has spIDs that are not in 'species' because these are ones where spID = NA
spI<-orderBy(~spID, spIndex[spIndex$spID %in% trIndex$spID,])
trI<-orderBy(~spID, trIndex[trIndex$spID %in% spIndex$spID,])

#i. Make sure there are no trait rows that refer to more than 1 spID
trIndex[is.na(trIndex$spID),'spnam'] #these trait species names refer to more tha 1 spID or have 'NA' in the spID reference column

#i. Same unique spIDs? 
identical(spI[,'spID'], trI[,'spID']) #if TRUE, then yes, unique spIDs are the same

#ii. Same species names for each unique spID?
sum(!as.character(spI[,'spName']) == as.character(trI[,'spnam'])) # should be 0, if not then species names are not identical
nonMatchingSPID<-data.frame(species=spI[!as.character(spI[,'spName']) == as.character(trI[,'spnam']),], 
                            traits=trI[!as.character(spI[,'spName']) == as.character(trI[,'spnam']),])
nonMatchingSPID #I'm okay with all of these...




## PART III. Check that Xsp data match 'species' spIDs and species names ####################################################

##A. Check that unlisted 'trait' species match 'species' spIDs and species names
check1<-list()
check2<-list()
check3<-list()
i<-0
for(i in 1:length(CATS)){
  
  #select correct df from the list
  expanded<-traits.Xsp.expandedList[[i]]
  expanded
  
  #set up
  trIndex<-ddply(expanded, ~spID, summarise, traitSpName = unique(traitSpName), spnam = GenusSpecies)
  spIndex<-species[,c('spID','spName')]
  check1[[i]]<-sum(!trIndex$spID %in% spIndex$spID) #if 0, then all trIndex spIDs are listed in species
  spI<-orderBy(~spID, spIndex[spIndex$spID %in% trIndex$spID,])
  trI<-orderBy(~spID, trIndex[trIndex$spID %in% spIndex$spID,])
  
  #checks
  #i. Same unique spIDs? 
  check2[[i]]<-sum(!spI[,'spID'] ==  trI[,'spID']) # if 0, then spIDs match
  
  #ii. Same species names for each unique spID?
  check3[[i]]<-sum(!spI[,'spName'] == trI[,'spnam'], na.rm=T) # this should be 0.  If not, that measures that the species names are not identical
}
check1 #all need to be 0
check2
check3








