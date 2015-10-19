#synthesis/script_c.R
#Calculate the community-weighted mean for plant traits in invaded and native areas

### Load libraries and fxns ####################################################
library(plyr) 
source('CODE/cwm/fxn_FillTable_cover.R')




### Make empty table ####################################################
#View(species)

# A. Assign column names
coverOfInterest<-c('Inv', 'Nat')
coverColNams<-c(paste("mean_",coverOfInterest, sep=''),
                paste("var_",coverOfInterest, sep=''),
                paste("n_",coverOfInterest, sep=''),
                "unit",
                "qualityMeas",
                "qualityNumSp")
#length(coverColNams)

# B. Assign row ids
spColNams<-c('spID','GenusSpecies','Genus','Species')
spCols<-species[,spColNams]

# C. Set up empty dataframe
tmp<-mat.or.vec(nr=dim(spCols[1]), nc=length(coverColNams))
tmp[tmp==0] <- NA
emptyDf<-data.frame(spCols, tmp)
colnames(emptyDf)<-c(spColNams,coverColNams)

# D. Get rid of rows with spID or Genus identifer == NA
emptyDf[is.na(emptyDf$spID),] #none
emptyDf[is.na(emptyDf$Genus),] #1
emptyDf[is.na(emptyDf$Species),] #same as above
emptyDf<-emptyDf[!is.na(emptyDf$Genus),] #fix this.
#View(emptyDf)




### Fill with spID values from the paper traits ####################################################
#View(cover.sp)

# A. Pull out the matching traitCat rows in traits
unique(cover.sp$covCat) #good. only 1 covCat
unique(cover.sp$stdunit) #good. only 1 unit
cover.sp.summ<-ddply(cover.sp, ~spID, summarise,
                     meanInv= stdmeanInv,
                     varInv= stdvarInv,
                     nInv=covInvN,
                     meanNat= stdmeanNat,
                     varNat= stdvarNat,
                     nNat= covNatN,
                     unit= stdunit,
                     qualityMeas= covQuality,
                     qualityNumSp= covNumSpp)

#View(cover.sp.summ)
sum(is.na(cover.sp.summ$spID)) # this should be 0.  If not, then there is an 'NA' under spID and need to get rid of that row


# B. For each spID, enter the data into the appropriate column of emptyDf
spIDcover<-FillTable.cover(toBeFilled=emptyDf, 
                           fillWithThis=cover.sp.summ, 
                           identifier='spID')
#View(spIDcover)
colsToAmmend<-colnames(spIDcover)[-(1:4)]
colnames(spIDcover)[-(1:4)]<-paste('cover',colsToAmmend,sep='_') #rename columns
#View(spIDcover)

# C. Summarize
sparse.fill_cover<-SparseSumm.cover(spIDcover)
sparse.fill_cover



