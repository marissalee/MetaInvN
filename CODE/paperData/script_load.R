#CODE/paperData/script_load.R
#Load raw paper data

### Load helper fxns and libraries ####################################################
library(doBy)
library(plyr)




### Load paper data ####################################################

papers<-read.table("DATA/papers.txt",header=TRUE,sep="\t", fill=TRUE, quote = "", na.strings="", stringsAsFactors = FALSE) 
#View(papers)

observations<-read.table("DATA/observations.txt",header=TRUE,sep="\t", fill=TRUE, na.strings="", stringsAsFactors = FALSE) 
#View(observations)

species<-read.table("DATA/species.txt",header=TRUE,sep="\t", na.strings="", stringsAsFactors = FALSE) 
#View(species)

measures<-read.table("DATA/measures.txt",header=TRUE,sep="\t", na.strings="", stringsAsFactors = FALSE)
#View(measures)

measAgg<-read.table("DATA/measAgg.txt",header=TRUE,sep="\t", na.strings="",  stringsAsFactors = FALSE)
#View(measAgg)

cover<-read.table("DATA/cover.txt",header=TRUE,sep="\t", na.strings="",  stringsAsFactors = FALSE) 
#View(cover)

covAgg<-read.table("DATA/covAgg.txt",header=TRUE,sep="\t", na.strings="",  stringsAsFactors = FALSE) 
#View(covAgg)

traits<-read.table("DATA/traits.txt",header=TRUE,sep="\t", na.strings="",  stringsAsFactors = FALSE) 
#View(traits)

traitAgg<-read.table("DATA/traitAgg.txt",header=TRUE,sep="\t", na.strings="",  stringsAsFactors = FALSE) 
#View(traitAgg)




### 1. Fix the format of NAs ####################################################
#Currently, NAs that I entered into the raw excel files are coded as "NA", so R sees them as factors
#FixNAs using this fxn
FixNAs<-function(dat){
  dat2 <- as.matrix(dat) # convert to matrix 
  y <- which(dat == 'NA') # get index of 'NA' values 
  dat2[y] <- NA # replace with NA
  dat.fixed<-as.data.frame(dat2)
  
  return(dat.fixed)
}
observations <- FixNAs(observations)
measures <- FixNAs(measures)
measAgg <- FixNAs(measAgg)
cover <- FixNAs(cover)
covAgg <- FixNAs(covAgg)
traits <- FixNAs(traits)
traitAgg <- FixNAs(traitAgg)




### 2. Fix ID keys (obsID, AggNum, paperID, aggID, spID ####################################################
source('CODE/paperData/script_load_fixIDs.R') 
#TASK= makes IDs numeric, makes sure there are no dups, creates missing ID cols
#NEEDS= observations, measures, measAgg, cover, covAgg, traits, traitAgg
#MAKES= same as above




### 3. Make sure data values numeric ####################################################
source('CODE/paperData/script_load_fixNumeric.R') 
#TASK= makes data cols numeric
#NEEDS= measures, measAgg, cover, covAgg, traits, traitAgg
#MAKES= same as above
#Note: warning messages 'NAs introduced by coercion' are because these data columns had 'AGG' as a placeholder



### 4. Fix white spaces in species names in 'species', 'traits', 'cover' ####################################################
trim <- function (x) gsub("^\\s+|\\s+$", "", x) # returns string w/o leading or trailing whitespace
species$spName<-trim(as.character(species$spName))
traits$traitSpName<-trim(as.character(traits$traitSpName))
cover$covDescript<-trim(as.character(cover$covDescript))



### 4b. Identify observations that have at least 1 focal & exotic & invasive species, store as a new column in observations
unique(species$spInvasive)
unique(species$spExotic)
unique(species$spFocal)
summ.spp <- ddply(species,~obsID,summarise, 
                  numInv= sum(spInvasive=='invasive'),
                  numExot= sum(spExotic=='exotic'),
                  numFocal= sum(spFocal=='focal'))
dim(summ.spp)
observations<-merge(observations, summ.spp, by='obsID')
#View(observations)

### 5. Check species names in 'species'. Add cols that identify: GenusSpecies, Genus, Species columns ####################################################
GenusSpecies<-species$spName

##A. Update the problematic species names 
test<-ldply(strsplit(GenusSpecies," "), length) #how many 'words' are in each?
issues<-which(test > 2) #record the row numbers with more than 2 'words'
GenusSpecies[issues] #print the potential problems
GenusSpecies[which(GenusSpecies=='Typha x glauca')]<-'Typha glauca' # I realize that this is a hybrid, but this makes the coding easier
GenusSpecies[which(GenusSpecies=='Phragmites australis americanus')]<-'Phragmities australis' # I realize that this is a hybrid, but this makes the coding easier
GenusSpecies[which(GenusSpecies=='native grasses and forbs')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='exotic grasses and forbs')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='native understory sp')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='native canopy sp')]<-NA # Cannot be used because need species names to match with trait values
GenusSpecies[which(GenusSpecies=='dune system herbs and shrubs p211')]<-NA # Cannot be used because need species names to match with trait values

##B. Insert cleaned genus+species names as a new column in 'species'
species$GenusSpecies<-GenusSpecies
species$Genus<-sub(patter=" .*",replacement="",x=species$GenusSpecies, perl=T)
species$Species<-sub(patter=".* ",replacement="",x=species$GenusSpecies, perl=T)
#View(species)




### 6. Closer look at the structure of 'cover' ####################################################
source('CODE/paperData/script_load_checkCover.R') 
#TASK= Make sure each observations has a cover measurement, specifically a sp_plantcov measurement. Make an expanded subset for cover measures on >1sp
#NEEDS= cover
#MAKES= cover.Xsp.expanded




### 7. Closer look at the structure of 'traits' ####################################################
source('CODE/paperData/script_load_checkTraits.R') 
#TASK= Make an expanded subset for trait measures on >1sp
#NEEDS= traits
#MAKES= traits.Xsp.expandedList




### 8. Closer look at the structure of 'measures' ####################################################
#identify each row with measEntryID2
measures$measEntryID2<-paste(measures$obsID, as.numeric(measures$measEntryID), sep=".") #identify each row
n<-ddply(measures, ~measEntryID2, summarize, n=length(unique(obsID)))[,'n']
sum(n!=1) # if this is 0, it means that there are no duplicate measEntryID2s

#Make sure each observation has only 1 value for a given measurement type
#summarize dataset by unique obsID+measCats so that data is not duplicated (multiple traits per obsID+measCat)
summ<-ddply(measures, ~obsID+measCat, summarize, uniq1 = length(unique(measEntryID2)))
sum(summ$uniq1!=1) # if this is not 0, then there are duplicat obsID+measCat combos
tab<-summ[summ$uniq1!=1,]
tab$reason<-0
tab$badMEID2<-NA

i<-0
for(i in 1:dim(tab)[1]){
  curr.obsID<-tab[i,'obsID']
  curr.measCat<-tab[i,'measCat']
  
  tmp<-measures[measures$obsID == curr.obsID & measures$measCat == curr.measCat,]
  if(sum(grepl("m2",tmp$measUnit)) != 0){
    tab[i,'reason']<-"m2"
    tab[i,'badMEID2']<-tmp[grepl("m2",tmp$measUnit),'measEntryID2']
  }
  if(sum(grepl("resin",tmp$measUnit)) != 0){
    tab[i,'reason']<-"resin"
    tab[i,'badMEID2']<-tmp[grepl("resin",tmp$measUnit),'measEntryID2']
  }
  if(curr.obsID != 7.01 & sum(grepl("soil pore water",tmp$measMethod)) != 0){
    tab[i,'reason']<-"soil pore water"
    tab[i,'badMEID2']<-tmp[grepl("soil pore water",tmp$measMethod),'measEntryID2']
  }
  if(curr.obsID == 127.01){
    tab[i,'reason']<-"lab incubation"
    tab[i,'badMEID2']<-tmp[grepl("lab incubation",tmp$measMethod),'measEntryID2']
  }
  if(curr.measCat == 'litterbiom'){
    tab[i,'reason']<-"same values"
    tab[i,'badMEID2']<-tmp[2,'measEntryID2']
  }
}
tab

#remove badMEID2 measure rows
measures1<-measures[!measures$measEntryID2 %in% tab$badMEID2,]

summ<-ddply(measures1, ~obsID+measCat, summarize, uniq1 = length(unique(measEntryID2)))
sum(summ$uniq1!=1) # good. no more duplicate measures


#remove badMEID2 measAgg rows
bagAggs<-measures[measures$measEntryID2 %in% tab$badMEID2,'aggID']
measAgg1<-measAgg[!measAgg$aggID %in% bagAggs,]



