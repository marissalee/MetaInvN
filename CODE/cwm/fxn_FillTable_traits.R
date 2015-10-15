#fxn_FillTable_traits.R
# Fxn to fill emptyDf with info based on different identifiers




### Function to fill emptyDF with info based on different identifiers ####################################################
#column names for 'fillWithThis' need to be 'mean','var','n','unit'
FillTable<-function(toBeFilled, fillWithThis, identifier){
  # For each identifier and traitCat, enter the traitCat data into the appropriate column of toBeFilled
  IDENTIFIER<-unique(fillWithThis[,identifier])
  i<-0
  for(i in 1:length(IDENTIFIER)){
    
    #subset fillWithThis by identifier
    tmp2<-fillWithThis[fillWithThis[,identifier] == IDENTIFIER[i],]
    
    #identify what traitCats are there
    tCats<-unique(tmp2$traitCat)
    
    #fill-in toBeFilled elements based on what tCats are present
    if(sum(tCats == 'sp_cn')==1){
      mvnu<-tmp2[tmp2$traitCat == 'sp_cn',c('mean','var','n','unit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_cn','var_cn','n_cn')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_cn')]<-as.character(mvnu$unit) 
      
    }
    if(sum(tCats == 'sp_percN')==1){
      mvnu<-tmp2[tmp2$traitCat == 'sp_percN',c('mean','var','n','unit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_percN','var_percN','n_percN')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_percN')]<-as.character(mvnu$unit)  
    }
    if(sum(tCats == 'sp_littercn')==1){
      mvnu<-tmp2[tmp2$traitCat == 'sp_littercn',c('mean','var','n','unit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_littercn','var_littercn','n_littercn')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_littercn')]<-as.character(mvnu$unit)  
    }
    if(sum(tCats == 'sp_litterpercN')==1){
      mvnu<-tmp2[tmp2$traitCat == 'sp_litterpercN',c('mean','var','n','unit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_litterpercN','var_litterpercN','n_litterpercN')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_litterpercN')]<-as.character(mvnu$unit) 
    }
    
  }
  
  return(toBeFilled)
}




### Function to summarize how populated the datatable is ####################################################
library(reshape2)
library(plyr)
SparseSumm<-function(filledDf){
  m.filledDf<-melt(filledDf, id.var=1:4) #melt data
  m.filledDf[,c('aggty','trait')]<-ldply(strsplit(as.character(m.filledDf$variable),"_"), rbind.fill) #create new identifier columns
  sparse.fill<-ddply(m.filledDf, ~aggty+trait, summarise, #summarize by those new identifier columns
                      numNA = sum(is.na(value)),
                      numNotNA = sum(!is.na(value)))
  
  return(sparse.fill)
}




### Function to fill emptyDF with info based on different identifiers (from tryGX type df) ####################################################
#column names for 'fillWithThis' need to be 'mean','var','n','unit'
FillTable.try<-function(toBeFilled, fillWithThis, identifier){
  # For each identifier and traitCat, enter the traitCat data into the appropriate column of toBeFilled
  IDENTIFIER<-unique(fillWithThis[,identifier])
  IDENTIFIER
  i<-0
  for(i in 1:length(IDENTIFIER)){
    
    #subset fillWithThis by identifier
    tmp2<-fillWithThis[fillWithThis[,identifier] == IDENTIFIER[i],]
    
    #identify what traitCats are there
    tCats<-unique(tmp2$traitCat)
    
    #fill-in toBeFilled elements based on what tCats are present
    if(sum(tCats == 'cn')==1){
      mvnu<-tmp2[tmp2$traitCat == 'cn',c('mean','var','n','stdunit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_cn','var_cn','n_cn')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_cn')]<-c(as.character(mvnu$stdunit))
    }
    if(sum(tCats == 'percN')==1){
      mvnu<-tmp2[tmp2$traitCat == 'percN',c('mean','var','n','stdunit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_percN','var_percN','n_percN')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_percN')]<-c(as.character(mvnu$stdunit))
    }
    if(sum(tCats == 'littercn')==1){
      mvnu<-tmp2[tmp2$traitCat == 'littercn',c('mean','var','n','stdunit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_littercn','var_littercn','n_littercn')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_littercn')]<-c(as.character(mvnu$stdunit)) 
    }
    if(sum(tCats == 'litterpercN')==1){
      mvnu<-tmp2[tmp2$traitCat == 'litterpercN',c('mean','var','n','stdunit')]
      rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
      toBeFilled[rows,c('mean_litterpercN','var_litterpercN','n_litterpercN')] <-mvnu[c('mean','var','n')]
      toBeFilled[rows,c('unit_litterpercN')]<-c(as.character(mvnu$stdunit))
    }
    
  }
  
  return(toBeFilled)
}










