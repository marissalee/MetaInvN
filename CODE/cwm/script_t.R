#synthesis/script_t1_emptyDf.R
#Calculate the community-weighted mean for plant traits in invaded and native areas

### Load libraries and fxns ####################################################
library(plyr) #for ddply
source('rmdCode/cwm/fxn_FillTable_traits.R') #for FillTable, FillVarTy, SparseSumm




### Make empty table ####################################################
#View(species)

# A. Assign column names
traitsOfInterest<-c('cn', 'percN', 'littercn', 'litterpercN')
traitColNams<-c(paste("mean_",traitsOfInterest, sep=''),
           paste("var_",traitsOfInterest, sep=''),
           paste("n_",traitsOfInterest, sep=''),
           paste("unit_",traitsOfInterest, sep=''))
#length(traitColNams)

# B. Assign row ids
spColNams<-c('spID','GenusSpecies','Genus','Species')
spCols<-species[,spColNams]

# C. Set up empty dataframe
tmp<-mat.or.vec(nr=dim(spCols[1]), nc=length(traitColNams))
tmp[tmp==0] <- NA
emptyDf<-data.frame(spCols, tmp)
colnames(emptyDf)<-c(spColNams,traitColNams)

# D. Get rid of rows with spID or Genus identifer == NA
emptyDf[is.na(emptyDf$spID),] #none
emptyDf[is.na(emptyDf$Genus),] #0
emptyDf[is.na(emptyDf$Species),] #same as above
emptyDf<-emptyDf[!is.na(emptyDf$Genus),] #fix this.
#View(emptyDf)





### Fill with spID values from the paper traits ####################################################
#View(traits)

# A. Pull out the matching traitCat rows in traits
tmp<-ddply(traits, ~spID+traitCat, summarise,
           mean = mean_std,
           var= var_std,
           n= n,
           unit= stdunit)
trait.summ<-tmp[!is.na(tmp$spID),] # Get rid of the row if the identifier has an 'NA' in it
#trait.summ
#Problem: spID = 25.06.1 and 25.07.1 have 2 values for sp_litterpercN
#How the Problem was detected: recieved this error message 'In `[<-.data.frame`(`*tmp*`, emptyDf$spID == as.character(SPID[i]), replacement element 3 has 2 rows to replace 1 rows' when going through the loop below.
#Solution: There was probably a typo in 'traits' where the traitSpEntryID was entered incorrectly, so that the same ID was given to 2 different species.  This has been changed in the raw datafile

# B. For each spID and traitCat, enter the traitCat data into the appropriate column of emptyDf
fill_spID<-FillTable(toBeFilled=emptyDf, 
                     fillWithThis=trait.summ, 
                     identifier='spID')
#View(fill_spID)

# C. Summarize
sparse.fill_spID<-SparseSumm(fill_spID)
sparse.fill_spID




### Fill with species-specific values from tryData ####################################################
#View(tryGS.summ)

# A. Rename columns so that they match FillTable.try()
colnames(tryGS.summ)[which(colnames(tryGS.summ)=='AccGenusSpecies')]<-'GenusSpecies'
colnames(tryGS.summ)[which(colnames(tryGS.summ)=='traitOI')]<-'traitCat'

# B. Fill: For each GenusSpecies and traitOI, enter the traitOI data into the appropriate column of emptyDf
fill_tryGS<-FillTable.try(toBeFilled=emptyDf, 
                          fillWithThis=tryGS.summ, 
                          identifier='GenusSpecies')
#View(fill_tryGS)

# C. Summarize
sparse.fill_tryGS<-SparseSumm(fill_tryGS)
sparse.fill_tryGS




### Fill with genus-specific values from tryData ####################################################
#View(tryGX.summ)

# A. Rename columns so that they match FillTable.try()
colnames(tryGX.summ)[which(colnames(tryGX.summ)=='AccGenus')]<-'Genus'
colnames(tryGX.summ)[which(colnames(tryGX.summ)=='traitOI')]<-'traitCat'

# B. Fill: For each GenusSpecies and traitOI, enter the traitOI data into the appropriate column of emptyDf
fill_tryGX<-FillTable.try(toBeFilled=emptyDf, 
                          fillWithThis=tryGX.summ, 
                          identifier='Genus')
#View(fill_tryGX)

# C. Summarize
sparse.fill_tryGX<-SparseSumm(fill_tryGX)
sparse.fill_tryGX




### Sequentially merge ####################################################
#View(fill_spID)
#View(fill_tryGS)
#View(fill_tryGX)

#Identify the data frames that need to be merged
x<-fill_spID
y<-fill_tryGS
z<-fill_tryGX

#Merge data frames; assume that fill_spID (x) is better than fill_tryGS (y) is better than fill_tryGX (z)
add.x0<-x # Record original x
x[which(is.na(x),arr.ind=TRUE)] <- y[which(is.na(x),arr.ind=TRUE)] # Replace na elements in fill_spID (x) with elements from fill_tryGS (y)
add.x0y1<-x # Record x after adding y
x[which(is.na(x),arr.ind=TRUE)] <- z[which(is.na(x),arr.ind=TRUE)] # Replaces remaining na elements in fill_spID (x) with elements from fill_tryGX (z)
add.x0y1z2<-x # Record x after adding y and then adding z

#Record the quality of each value; subset just the value columns (no id columns) and replace values with the appropriate data quality name
valueCols<-c('mean_cn','mean_percN','mean_littercn','mean_litterpercN')
x0<-add.x0[,valueCols] 
x0[!is.na(x0)]<-'spID'
x0y1<-add.x0y1[,valueCols]
x0y1[!is.na(x0y1)]<-'tryGS'
x0y1z2<-add.x0y1z2[,valueCols]
x0y1z2[!is.na(x0y1z2)]<-'tryGX'

#Merge the data-quality data frames just like before
names<-x0
names[which(is.na(names),arr.ind=TRUE)] <- x0y1[which(is.na(names),arr.ind=TRUE)] # Replace na elements in x0 with elements from x0y1
names[which(is.na(names),arr.ind=TRUE)] <- x0y1z2[which(is.na(names),arr.ind=TRUE)] # Replaces remaining na elements in x0 with elements from x0y1z0
colnames(names)<-paste("quality", valueCols, sep="_")

#Horizontally merge names onto x
spIDtraits<-data.frame(x, names)
#View(spIDtraits)

#Update the df so that the values are coded as numeric
FixVals.tr<-function(df,these){
  i<-0
  for(i in 1:length(these)){
    this<-which(colnames(df)==these[i])
    if(length(this)>1){print("warning")}
    colname<-colnames(df)[this]
    df[,colname]<-as.numeric(as.character(df[,colname]))
  }
  return(df)
}
#str(spIDtraits)
tmp<-colnames(spIDtraits)[c(grep("var_",colnames(spIDtraits)), grep("n_", colnames(spIDtraits)))]
changeColNams<-tmp[!grepl('quality',tmp)]
spIDtraits<-FixVals.tr(df=spIDtraits, these=changeColNams)
#str(spIDtraits)








