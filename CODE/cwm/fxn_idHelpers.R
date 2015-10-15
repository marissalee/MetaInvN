#fxn_idHelpers.R
# Fxns to help organize IDs across dataframes




### Add species and genus columns to df using spID in 'species' ####################################################
Add.spID.Names<-function(df){
  df[,c('GenusSpecies','Genus','Species')]<-rep(NA, dim(df)[1]) #set up slots to put the species info
  SPID<-as.character(unique(df$spID)) #list of spIDs in the extended df to loop through
  i<-0
  for(i in 1:length(SPID)){
    row<-species[which(species$spID == SPID[i]),c('GenusSpecies','Genus','Species')]
    df[df$spID == SPID[i],c('GenusSpecies','Genus','Species')] <- row
  }
  return(df)
}




### Add unit, dataName, and traitOI columns to df using dataID in 'tryData_traitKey' ####################################################
AddTraitOI <- function(df, template){
  df[,c('unit','dataName','traitOI')]<-rep(NA, dim(df)[1]) #set up slots to put the template info
  DATAID<-unique(df$DataID) #list of dataIDs in the extended df to loop through
  i<-0
  for(i in 1:length(DATAID)){
    subrow<-which(template$dataID == DATAID[i])
    df[df$DataID == DATAID[i],c('unit')] <- as.character(template[subrow,'unit'])
    df[df$DataID == DATAID[i],c('dataName')] <- as.character(template[subrow,'dataName'])
    df[df$DataID == DATAID[i],c('traitOI')] <- as.character(template[subrow,'traitOI'])
  }
  return(df)
}


