#synthesis/script_cwm_3.R
#Calculate relative abundance and ammend values to the trait*relabund dataframes


#View(df.list[[1]])

#For each trait dataframe and obsID, calculate relative abundance for each spID with trait and cover data in invaded and native areas
#loop through each trait data frame in df.list; store an updated list of trait data frames
df.list2<-list()
t<-0
for(t in 1:length(df.list)){
  
  #identify column names for this trait's df
  includeTraitCol<-paste("include", traitsOfInterest[t], sep="_") #identify the column name that holds the NAs
  
  #subset this trait's df
  df<-df.list[[t]]
  df[,c('relabund_InvArea','relabund_NatArea', 'relabund_InvSpInvArea','relabund_notes')]<-rep(NA, dim(df)[1]) #make columns to hold the relative abundance values and notes
  
  #loop through each obsID
  OBSID<-unique(df$obsID)
  o<-0
  for(o in 1:length(OBSID)){
    
    #1. Identify rows in obsID that have all pieces of data
    rows<-df$obsID==OBSID[o] & df[,includeTraitCol]==1
    rows
    # Calculate the relative abundance for each spID in that obsID. The relative abundances of all spIDs in that obsID should sum to 1
    # NOTE: spcover of the species in an obsID might not add to 100% if ... 
    # A. Not all of the species were measured
    # B. Bareground was included in the measurement
    # C. Cover values were measured at multiple levels - the understory and canopy
    #pull out the total percent cover based on these species in invaded and native areas
    coverInvArea.total<-sum(df[rows, 'cover_mean_Inv'], na.rm=T) 
    coverNatArea.total<-sum(df[rows, 'cover_mean_Nat'], na.rm=T)  
    #if there is 0 cover, make a note
    if(coverNatArea.total==0){df[rows,'relabund_notes']<-'Native area has 0 cover, so cannot calculate relabund_NatArea'}
    #do the calculation and save the data in the pre-made columns of the df
    df[rows,'relabund_InvArea']<-df[rows,'cover_mean_Inv'] / coverInvArea.total *100 #calculate the relative abundance of each species based on the total percent cover measured on the species included
    df[rows,'relabund_NatArea']<-df[rows,'cover_mean_Nat'] / coverNatArea.total *100
    
    #2. Identify rows in obsID that have all pieces of data AND are invasive species
    invrows<-df$obsID==OBSID[o] & df[,includeTraitCol]==1 & df$spInvasive=='invasive'
    # Calculate the relative abundance for each invasive spID in that obsID. The relative abundances of all invasive spIDs in that obsID should sum to 1
    # NOTE: spcover of the invasive species in an obsID might not add to 100% if ... 
    # A. see reasons above
    # B. there are more than just invasive species in that area
    #pull out the total percent cover based on these species in invaded and native areas
    coverInvSpInvArea.total<-sum(df[invrows, 'cover_mean_Inv'])  
    #do the calculation and save the data in the pre-made columns of the df
    df[invrows,'relabund_InvSpInvArea']<-df[invrows,'cover_mean_Inv'] / coverInvSpInvArea.total *100 #calculate the relative abundance of each species based on the total percent cover measured on the species included
    
  }
  
  # save a df for each trait
  df.list2[[as.character(traitsOfInterest[t])]]<-df
}
#str(df.list2)
#View(df.list2[['cn']])

