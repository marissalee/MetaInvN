#script_spcover.R
#Calculate species cover in native and invaded areas. 
#NOTES: Cumulative species cover cannot exceed 100%.  Cumulative species cover can be less than 100% because of bare ground.


### Load libraries ####################################################
require(doBy) #for orderBy
require(reshape) #for untable()
#source('rmdCode/paperData/fxn_loadHelpers.R') 
source('CODE/cwm/fxn_idHelpers.R') 



### Check out data ####################################################
#View(cover)
#View(observations)
NoCov1<-!unique(cover$obsID) %in% unique(observations$obsID) # Does each observation have a cover entry?
unique(cover$obsID)[NoCov1=='TRUE'] # these obsIDs in the cover file do not have a correponding obsID in the observations file
NoCov2<-!unique(observations$obsID) %in% unique(cover$obsID) # Does each observation have a cover entry?
unique(observations$obsID)[NoCov2=='TRUE'] # these obsIDs in the observations file do not have a corresponding obsID in the cover file



### Subset cover data so that we are only using % (and will be only using the std versions) ####################################################
cover.sub<-cover[which(cover$stdunit=='%'),]
#again, make sure each observation has a cover sp %
NoCov1<-!unique(cover.sub$obsID) %in% unique(observations$obsID) # Does each observation have a cover entry?
unique(cover.sub$obsID)[NoCov1=='TRUE'] # these obsIDs in the cover file do not have a correponding obsID in the observations file
NoCov2<-!unique(observations$obsID) %in% unique(cover.sub$obsID) # Does each observation have a cover entry?
unique(observations$obsID)[NoCov2=='TRUE'] # these obsIDs in the observations file do not have a corresponding obsID in the cover file




### Subset cover data by the number of species included in the cover value ####################################################
cover.sub.1sp<-cover.sub[cover.sub$covNumSpp == '1 species',]
cover.sub.Xsp<-cover.sub[cover.sub$covNumSpp == '>1 species',]
dim(cover.sub.1sp)
dim(cover.sub.Xsp)



### Check cover.sub.1sp ####################################################
cover.sub.1sp$numsp<-ldply(strsplit(as.character(cover.sub.1sp$covSpEntryID), ','), length)[,1] #make a column that holds the number of species
sum(cover.sub.1sp$numsp != 1) # this should be 0.  If it is not, then there is an entry with >1 spEntryID (mislabeled)
cover.sub.1sp[cover.sub.1sp$numsp!=1,] # none. good.




# Check cover.sub.Xsp ####################################################
tmp<-strsplit(as.character(cover.sub.Xsp$covSpEntryID), ',') #split the list of sp#s in covSpEntryID
cover.sub.Xsp$numsp<-ldply(strsplit(as.character(cover.sub.Xsp$covSpEntryID), ','), length)[,1] #make a column that holds the number of species
sum(cover.sub.Xsp$numsp == 1) # this should be 0.  If it is not, then there is an entry with only 1 spEntryID although it is labeled as an entry with >1sp
cover.sub.Xsp[cover.sub.Xsp$numsp==1,'covDescript'] # its ok; although there is only 1 spEntryID, it is supposed to be labelled as >1sp




### For cover data where there is more than 1 species, divide the cover value by the species assigned in the 'covSpEntryID' and expand the table ####################################################
#View(cover.sub.Xsp)
# strsplit the covSpEntryID string and count the number of species (numsp)
splist<-strsplit(as.character(cover.sub.Xsp$covSpEntryID), ',')
# expand the cover dataframe by the numsp value and update the column with the covSpEntryID
expanded<-untable(cover.sub.Xsp, num=cover.sub.Xsp[,'numsp']) #need library(reshape), use the column with the number of species to expand the table
dim(expanded)
expanded$covSpEntryID<-as.numeric(unlist(splist)) #update the covSpEntryID that is specific to each row in this expanded table
#View(expanded)


# divide each cover value (inv and nat) by the numsp to get the new cover value (ex: new.covInvMean)
# remember that ... Var(aX) = a^2 * Var(X)
meancols<-c('stdmeanInv','stdmeanNat')
varcols<-c('stdvarInv','stdvarNat')
expanded[,meancols]<-expanded[,meancols] * (1/ expanded[,'numsp'])
expanded[,varcols]<-expanded[,varcols] * (1/ expanded[,'numsp'])^2


### Update expanded datatable with species name columns ####################################################

# add spID
expanded$spID<-paste(expanded$obsID, expanded$covSpEntryID, sep='.') #identify the spIDs
#check to make sure you created unique spIDs... 
tmp<-ddply(expanded, ~spID, summarise, n = length(spID))
tmp[tmp$n >1,] #which spIDs are duplicates? this should be 0

#expanded<-Add.spID.Names(expanded) # if this doesn't work, see below for one of those types of problems
# # Problem - there are spIDs that were created from spEntryID == NA... need to exclude these somehow
SPID<-unique(expanded$spID)
length(SPID)
numIDd<-SPID %in% species$spID # identify the position of spIDs referenced in the cover file based on where they are in the species file
SPID[!numIDd] # these are the spIDs that are not present in the species file
removeRows<-expanded$spID %in% SPID[!numIDd]
expanded1<-expanded[!removeRows,]
dim(expanded); dim(expanded1)
expanded<-Add.spID.Names(expanded1) # now this should work



# check spID names against those in 'species'
covIndex<-ddply(expanded, ~spID, summarise, covDescript = unique(covDescript), spnam = unique(GenusSpecies))
spIndex<-species[,c('spID','spName')]
sum(!covIndex$spID %in% spIndex$spID) #if 0, then all covIndex spIDs are listed in species

spI<-orderBy(~spID, spIndex[spIndex$spID %in% covIndex$spID,])
covI<-orderBy(~spID, covIndex[covIndex$spID %in% spIndex$spID,])
dim(spI)
dim(covI)

#check to see if spIDs match
sum(!spI[,'spID'] ==  covI[,'spID']) # if 0, then spIDs match

#check to see if sp names match, but first - fix white space around sp names in covI
# returns string w/o leading or trailing whitespace.. need to fix covI[,spnam]
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
covI[,'spnam']<-trim(as.character(covI[,'spnam']))
spI[,'spName']<-trim(as.character(spI[,'spName']))
sum(!as.character(spI[,'spName']) == as.character(covI[,'spnam']), na.rm=T) # this should be 0.  If not, that measures that the species names are not identical
nonMatchingSPID<-data.frame(species=spI[!as.character(spI[,'spName']) == as.character(covI[,'spnam']),], 
                            cover=covI[!as.character(spI[,'spName']) == as.character(covI[,'spnam']),])
nonMatchingSPID #none now.


### Update 1sp datatable with species name columns ####################################################

# add names based on cover$spID
cover.sub.1sp[,c('GenusSpecies','Genus','Species')]<-rep(NA, dim(cover.sub.1sp)[1]) #set up slots to put the species info
SPID<-as.character(unique(cover.sub.1sp$spID)) #list of spIDs in the extended df to loop through
i<-0
for(i in 1:length(SPID)){
  row<-species[which(species$spID == SPID[i]),c('GenusSpecies','Genus','Species')]
  cover.sub.1sp[cover.sub.1sp$spID == SPID[i],c('GenusSpecies','Genus','Species')] <- row
}




### Merge the expanded datatable and 1sp table use a new column to indicate cover value quality: covQuality2 == covNumSpp ####################################################
#make factors into characters
FactorsToChar<-function(df){
  tmp <- sapply(df, is.factor)
  df[tmp] <- lapply(df[,tmp], as.character)
  return(df)
}
cov.1sp<-FactorsToChar(cover.sub.1sp)
cov.Xsp<-FactorsToChar(expanded)
cover.sp<-rbind(cov.1sp,cov.Xsp)
#str(cover.sp)
#View(cover.sp)


detach("package:reshape",unload=TRUE) #for untable()




