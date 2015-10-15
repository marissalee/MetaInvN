#synthesis/script_traitBySpecies.R
#Make a traitBySpecies table from tryData


### Load libraries ####################################################
library(plyr)




### Update the species and genus list in paper data ####################################################

# 1. Look at the species names from the species dataframe
GenusSpecies<-as.character(species$spName)

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
GenusSpecies<-trim(GenusSpecies)

# 2. Update the problematic names and insert cleaned genus+species names as a new column
test<-ldply(strsplit(GenusSpecies," "), length) #how many 'words' are in each?
issues<-which(test > 2) #record the row numbers with more than 2 'words'
GenusSpecies[issues] #print the potential problems
GenusSpecies[which(GenusSpecies=='Typha x glauca')]<-'Typha glauca' # I realize that this is a hybrid, but this makes the coding easier
GenusSpecies[which(GenusSpecies=='Typha x glauca ')]<-'Typha glauca' # I realize that this is a hybrid, but this makes the coding easier
GenusSpecies[which(GenusSpecies=='Phragmites australis americanus')]<-'Phragmities australis' # I realize that this is a hybrid, but this makes the coding easier
GenusSpecies[which(GenusSpecies=='native grasses and forbs')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='exotic grasses and forbs')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='native understory sp')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='native canopy sp')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='dune system herbs and shrubs p211')]<-NA # Cannot be used because need species names to match with trait values

species$GenusSpecies<-GenusSpecies


# 3. Add an accepted genus and species column to the species dataframe
species$Genus<-sub(patter=" .*",replacement="",x=species$GenusSpecies, perl=T)
species$Species<-sub(patter=".* ",replacement="",x=species$GenusSpecies, perl=T)
#View(species)


### Update the species and genus list in the subsetted try data ####################################################

#1. Check to see if the AccSpeciesName column is clean
test<-ldply(strsplit(as.character(tryDataT.cu$AccSpeciesName), " "), length) #how many 'words' are in each?
issues<-which(test > 2) #record the row numbers with more than 2 'words' #warnings because there are weird characters in some of these strings...
#issues
#tryDataT.cu[12106,'AccSpeciesName']

# 2. Add an accepted genus and species column to the subsetted try dataframe
tryDataT.cu$AccGenus<-sub(patter=" .*",replacement="",x=tryDataT.cu$AccSpeciesName, perl=T)
tryDataT.cu$AccSpecies<-sub(patter=".* ",replacement="",x=tryDataT.cu$AccSpeciesName, perl=T)
tryDataT.cu$AccGenusSpecies<-paste(tryDataT.cu$AccGenus, tryDataT.cu$AccSpecies)




### Pull relevant trait data using a unique GENUS-SPECIES list from the paper data ####################################################

# 1. Unique Genus+Species list from paper data
paperGenusSpecies<-unique(species$GenusSpecies)
length(paperGenusSpecies)

# 2. Subset tryDataT by the paper unique species list
tryGS<-tryDataT.cu[tryDataT.cu$AccGenusSpecies %in% paperGenusSpecies,]
#dim(tryGS)




### For the GENUS-SPECIES from the paper data did not find a match in the try data, pull GENUS info from try data ####################################################

# 1. Determine the GS matches, no matches
tryMatchGS<-paperGenusSpecies[paperGenusSpecies %in% tryDataT.cu$AccGenusSpecies]
tryNoMatchGS<-paperGenusSpecies[!paperGenusSpecies %in% tryDataT.cu$AccGenusSpecies]
numMatchGS<-length(tryMatchGS)
numNoMatchGS<-length(tryNoMatchGS)
numMatchGS
numNoMatchGS

# 2. Identify a unique GENUS list from the no matches
GX<-sub(patter=" .*",replacement="",x=tryNoMatchGS, perl=T)
paperGenus<-unique(GX)

# 3. Subset tryDataT by the paper unique genus list
tryGX<-tryDataT.cu[tryDataT.cu$AccGenus %in% paperGenus,]
#dim(tryGX)




### Aggregate across species and DataID (trait type) for tryGS and tryGX ####################################################
colnames(tryGS)
tryGS.summ <- ddply(tryGS,~ AccGenusSpecies + traitOI + stdunit, summarise, #dataID is more specific than traitID
              n = sum(!is.na(stdmean)),
              mean = mean(stdmean, na.rm=T),
              var = var(stdmean, na.rm=T),
              varty = "VAR")
tryGX.summ <- ddply(tryGX,~ AccGenus + traitOI + stdunit, summarise, #dataID is more specific than traitID
                    n = sum(!is.na(stdmean)),
                    mean = mean(stdmean, na.rm=T),
                    var = var(stdmean, na.rm=T),
                    varty = "VAR")
#View(tryGS.summ)
#View(tryGX.summ)

