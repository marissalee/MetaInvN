#script_cwm_4.R
#Calculate the cwm in invaded and native areas for each obsID; Keep track of associated parameters

### Load libraries and fxns ####################################################
source('CODE/paperData/fxn_1_Calcs.R') #for CalcMean




#loop through each trait dataframe
cwm.list<-list()
t<-0
for(t in 1:length(df.list2)){
  
  # Identify column names for this trait's df
  meanTraitCol<-paste("mean", traitsOfInterest[t], sep="_") #identify the column name that holds the trait mean for that spID
  varTraitCol<-paste("var", traitsOfInterest[t], sep="_") 
  nTraitCol<-paste("n", traitsOfInterest[t], sep="_") 
  unitTraitCol<-paste("unit", traitsOfInterest[t], sep="_")
  qualityTraitCol <- paste("quality_mean", traitsOfInterest[t], sep="_")
  includeTraitCol<-paste("include", traitsOfInterest[t], sep="_") #identify the column name that holds the NAs
  
  # Subset this trait's df
  df<-df.list2[[t]]
  
  #loop through each obsID
  cwm_store<-numeric(0)
  OBSID<-unique(df$obsID)
  o<-0
  for(o in 1:length(OBSID)){
    
    # Identify rows in obsID that have all pieces of data
    rows<-df$obsID==OBSID[o] & df[,includeTraitCol]==1
    #View(df[rows,])
    
    # Identify the presence of native and invasive species in this obsID
    spCounts<-ddply(df[df$obsID==OBSID[o],c('spInvasive','InvCovHere','NatCovHere',includeTraitCol)], ~spInvasive, summarise,
          nInvCov=sum(InvCovHere),
          nNatCov=sum(NatCovHere))
    occup<-c('invasive','native') %in% spCounts$spInvasive
    if(occup[1] == FALSE){spInv<-data.frame(nInvCov=0, nNatCov=0)}else{spInv<-spCounts[spCounts$spInvasive=='invasive',2:3]}
    if(occup[2] == FALSE){spNat<-data.frame(nInvCov=0, nNatCov=0)}else{spNat<-spCounts[spCounts$spInvasive=='native',2:3]}
    spCountsDF<-data.frame(spInv, spNat)
    colnames(spCountsDF)<-paste(rep(c('invadedArea','nativeArea'),2),rep(c('invasiveSp','nativeSp'), each=2), sep="_")
    #spCountsDF
    
    # community-weighted trait var and n (assuming that the relative abundance is known)
    tmp<-CalcMean(group.means=df[rows, meanTraitCol], 
              group.vars=df[rows, varTraitCol], 
              group.ns=df[rows, nTraitCol])
    var<-tmp$global.var
    nTr<-tmp$global.n
    
    # unit that the cwm trait values are in
    unit<-paste(unique(df[rows,unitTraitCol]), collapse="_") #need to go back and make it so that traits are all in the same unit before this... C:N should be molC/molN
    
    # if there is a relabundance note, then carry it over
    if(sum(!is.na(df[rows,'relabund_notes']))>0){
      relabund_note<-unique(df[rows,'relabund_notes'])
    } else{
      relabund_note<-NA
    }
    
    #loop through invaded and native areas
    AREA<-c('InvArea','NatArea')
    InvNatData<-numeric(0)
    a<-0
    for(a in 1:length(AREA)){
      
      #Calculate the cwm for the area
      abundColName<-paste('relabund',AREA[a],sep="_")
      rowsCov<-df$obsID==OBSID[o] & df[,includeTraitCol]==1 & df[,abundColName]!=0 & !is.na(df[,abundColName])#Pull out the rows where a species had some cover value greater than 0 in this area
      if(sum(rowsCov)==0){
        mean<-NA
      }else{
        mean<-sum(df[rowsCov, meanTraitCol] * (df[rowsCov,abundColName]/100))
      }
      
      #N
      nSp<-sum(rowsCov)
      
      #N cover quality: measured or BOSD
      nMeasCov<-sum(df[rowsCov,'cover_qualityMeas'] == 'measured')
      nBOSDCov<-sum(df[rowsCov,'cover_qualityMeas'] == 'BOSD')
      
      #N cover quality: 1 or >1 sp
      n1spCov<-sum(df[rowsCov,'cover_qualityNumSp']=='1 species')
      nXspCov<-sum(df[rowsCov,'cover_qualityNumSp']=='>1 species')
      
      #N trait quality: spID, tryGS, tryGX
      nOrigTr<-sum(df[rowsCov, qualityTraitCol] == 'spID')
      nTryGS<-sum(df[rowsCov, qualityTraitCol] == 'tryGS')
      nTryGX<-sum(df[rowsCov, qualityTraitCol] == 'tryGX')
      
      #Save everthing
      areaData<-c(mean, nSp, nMeasCov, nBOSDCov, n1spCov, nXspCov, nOrigTr, nTryGS, nTryGX)
      InvNatData<-c(InvNatData,areaData)
    }
    #InvNatData
    
    #add invasive sp in invaded areas
    #Calculate the cwm for the inv species in the invaded area
    abundColName<-'relabund_InvSpInvArea'
    rowsCov<-df$obsID==OBSID[o] & df[,includeTraitCol]==1 & df[,abundColName]!=0 & !is.na(df[,abundColName])#Pull out the rows where a species had some cover value greater than 0 in this area
    if(sum(rowsCov)==0){
      mean<-NA
    }else{
      mean<-sum(df[rowsCov, meanTraitCol] * (df[rowsCov,abundColName]/100))
    }
    
    #N
    nSp<-sum(rowsCov)
    
    #N cover quality: measured or BOSD
    nMeasCov<-sum(df[rowsCov,'cover_qualityMeas'] == 'measured')
    nBOSDCov<-sum(df[rowsCov,'cover_qualityMeas'] == 'BOSD')
    
    #N cover quality: 1 or >1 sp
    n1spCov<-sum(df[rowsCov,'cover_qualityNumSp']=='1 species')
    nXspCov<-sum(df[rowsCov,'cover_qualityNumSp']=='>1 species')
    
    #N trait quality: spID, tryGS, tryGX
    nOrigTr<-sum(df[rowsCov, qualityTraitCol] == 'spID')
    nTryGS<-sum(df[rowsCov, qualityTraitCol] == 'tryGS')
    nTryGX<-sum(df[rowsCov, qualityTraitCol] == 'tryGX')
    
    #Save everthing from invasive species in invaded areas
    InvspInvAreaData<-c(mean, nSp, nMeasCov, nBOSDCov, n1spCov, nXspCov, nOrigTr, nTryGS, nTryGX)
    values<-c(InvNatData,InvspInvAreaData)
    currCols<-c("mean", "nSp", "nMeasCov", "nBOSDCov", "n1spCov", "nXspCov", "nOrigTr", "nTryGS", "nTryGX")
    currCols2<-rep(currCols, 3)
    areaCols<-c(rep(AREA, each=length(currCols)), rep('InvSpInvArea',length(currCols)))
    valueNames<-paste(currCols2,areaCols, sep='_')
    
    
    #Save everything in a row, store that row of info
    tmp<-data.frame(value=values, valueName=valueNames, var, nTr, unit, relabund_note, spCountsDF) #row.names discarded; warning is ok
    obsIDvec<-rep(OBSID[o], dim(tmp)[1])
    obsIDInfo<-data.frame(obsID=obsIDvec, tmp)
    
    #print(obsIDInfo[obsIDInfo$valueName=='mean_InvArea' | obsIDInfo$valueName=='mean_NatArea' | obsIDInfo$valueName=='mean_InvSpInvArea' ,c('obsID','value')])
    
    cwm_store<-rbind(cwm_store,obsIDInfo)
  }
  
  #Save the whole trait dataframe into a list
  cwm.list[[as.character(traitsOfInterest[t])]]<-cwm_store
}
#ignore warnings, since they are about rownames
#str(cwm.list)
#View(cwm.list[['cn']])



