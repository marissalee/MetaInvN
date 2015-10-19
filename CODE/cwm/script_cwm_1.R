#script_cwm_1.R
#Prep trait x spID dataframes




# 1. Merge traits and cover by spID
spIDsyn<-merge(spIDtraits,spIDcover, by=c("spID","GenusSpecies","Genus","Species"))

# 2. Add an 'obsID' column to spIDsyn
tmp<-ldply(strsplit(as.character(spIDsyn$spID), '.', fixed=T), rbind.fill)[,1:2]
spIDsyn$obsID<-paste(tmp[,1], tmp[,2], sep=".")
sum(!spIDsyn$obsID %in% observations$obsID) # this should be 0.  If not, that means that there is an obsID in the spIDsyn data frame that doesn't exist in the observations data frame

# 3. Add an 'spInvasive','spGrowth','spHeight' columns to spIDsyn
spIDsyn1<-merge(spIDsyn,species, by=c('spID','obsID','GenusSpecies','Genus','Species'))
spIDsyn1[!spIDsyn1$GenusSpecies == spIDsyn1$spName,c('GenusSpecies','spName')] # check to make sure that the merge happened correctly.  Check.
#View(spIDsyn1)

# 4. For each trait, make a column that indicates if the species has 1) trait mean and a non-0 value for invaded cover OR native cover.
#loop through traitsOfInterest; store whether row has all 3 pieces of data
store<-numeric(0)
t<-0
for(t in 1:length(traitsOfInterest)){
  traitCol<-paste("mean",traitsOfInterest[t], sep="_")
  traitCol2<-paste("traitHere",traitsOfInterest[t], sep="_")
  logitmp<-!is.na(spIDsyn1[, c(traitCol,'cover_mean_Inv','cover_mean_Nat')]) & spIDsyn1[, c(traitCol,'cover_mean_Inv','cover_mean_Nat')] != 0 #is there a value there that is not 0
  spIDsyn1$InvCovHere<-logitmp[,'cover_mean_Inv']
  spIDsyn1$NatCovHere<-logitmp[,'cover_mean_Nat']
  spIDsyn1[,traitCol2]<-logitmp[,traitCol]
  
  traitNA<-logitmp[,traitCol] # TRUE == presence of a trait value
  tmp<-apply(logitmp[,c('cover_mean_Inv','cover_mean_Nat')], 1, sum) # add the presence of a value in inv and nat areas
  coverNA1<-tmp != 0 # TRUE == at least 1 value in inv or nat
  
  #if the only cover value is 0, then still can't use this observation
  tmp0<-spIDsyn1[,c('cover_mean_Inv','cover_mean_Nat')] == 0 # where are there 0s?
  tmpNA<-is.na(spIDsyn1[,c('cover_mean_Inv','cover_mean_Nat')]) #where are there NAs?
  tmp0s<-apply(tmp0, 1, function(x) sum(x, na.rm=T))
  tmpNAs<-apply(tmpNA, 1, function(x) sum(x))
  cNA1<-tmp0s == 2 # TRUE == cases where both inv and nat values are 0
  cNA2<-tmp0s == 1 & tmpNAs == 1# TRUE == cases where 1 inv/nat value is NA and 1 inv/nat value is 0
  coverVec<-coverNA1==TRUE & cNA1==FALSE & cNA2 == FALSE
  
  #evaluate trait along side coverVec to decide if the row qualifies
  includeVec<-coverVec == TRUE & traitNA == TRUE # TRUE == trait data AND non-zero cover data in either inv or nat area
  
  store<-cbind(store, includeVec)
}# TRUE == trait data AND non-zero cover data in either inv or nat area
colnames(store)<-paste("include",traitsOfInterest, sep="_") #update the col names
spIDsyn2<-data.frame(spIDsyn1,store) #append to spIDsyn1
#View(spIDsyn2)


# 5. For each trait, make a list of obsIDs that have at least 1 native OR invasive species with the include_trait == 1 (TRUE)
#loop through traitsOfInterest and obsIDs; store list of obsIDs that meet the criteria for each trait
obsid.list<-list()
t<-0
for(t in 1:length(traitsOfInterest)){
  
  #define trait cols
  traitCol<-paste("include", traitsOfInterest[t], sep="_")
  
  #loop through obsIDs
  store<-numeric(0)
  OBSID<-unique(spIDsyn2$obsID)
  o<-0
  for(o in 1:length(OBSID)){
    
    #subset rows with that obsID
    sub<-spIDsyn2[spIDsyn2$obsID == OBSID[o],] 
    
    #Invasive species
    subInv<-sub[sub$spInvasive=='invasive',traitCol]
    nInv<-sum(subInv==1)
    if(nInv!=0){lInv<-1} # Is there at least 1 species with all pieces of info (aka 0 NAs)? 0=No, 1=Yes
    if(nInv==0){lInv<-0}
    
    #Native species
    subNat<-sub[sub$spInvasive=='native',traitCol]
    nNat<-sum(subNat==1)
    if(nNat!=0){lNat<-1} # Is there at least 1 species with all pieces of info (aka 0 NAs)? 0=No, 1=Yes
    if(nNat==0){lNat<-0}
    
    #Is there at least 1 invasive OR native species with all pieces of info?
    if(sum(lInv,lNat)>=1){add.obsid<-OBSID[o]}else{add.obsid<-NA}
    
    #Save the vector of obsIDs for this trait
    store<-c(store,add.obsid)
  }
  #Save the vectors of obsIDs in a list
  obsid.list[[as.character(traitsOfInterest[t])]]<-store
}
#obsid.list




# 6. For each trait, subset spIDsyn2 by the obsIDs that have info to calculate cwm, make 4 dataframes
#loop through traitsOfInterest; store a list of dataframes
df.list<-list()
t<-0
for(t in 1:length(traitsOfInterest)){
  
  #subset obsIDs with all the necessary info for this trait
  obsid1<-obsid.list[[t]]
  
  #identify the column names for this trait
  meanCol<-paste('mean',traitsOfInterest[t], sep="_")
  varCol<-paste('var',traitsOfInterest[t], sep="_")
  nCol<-paste('n',traitsOfInterest[t], sep="_")
  unitCol<-paste('unit',traitsOfInterest[t], sep="_")
  qualityCol<-paste('quality_mean',traitsOfInterest[t], sep="_")
  includeCol<-paste('include',traitsOfInterest[t], sep="_")
  
  #subset the data by obsid1 (rows) and the relavant trait cols
  cols<-c('spID','obsID',
    'GenusSpecies','Genus','Species',
    'spInvasive','spGrowth','spHeight','spAnnPerenn',
    meanCol, varCol, nCol, unitCol, qualityCol, 
    'cover_mean_Inv','cover_mean_Nat', 'cover_var_Inv','cover_var_Nat','cover_n_Inv','cover_n_Nat',
    'cover_unit','cover_qualityMeas','cover_qualityNumSp',
    includeCol, 'InvCovHere', 'NatCovHere')
  
  sub1<-spIDsyn2[spIDsyn2$obsID %in% obsid1[!is.na(obsid1)],colnames(spIDsyn2) %in% cols]
  
  #save the subsetted data
  df.list[[as.character(traitsOfInterest[t])]]<-sub1
}
names(df.list)<-traitsOfInterest
#View(df.list[['cn']])




