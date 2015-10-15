#fxn_FillTable_cover.R
# Fxn to fill emptyDf with info based on different identifiers




### Function to fill emptyDF with info based on different identifiers ####################################################
FillTable.cover<-function(toBeFilled, fillWithThis, identifier){
  IDENTIFIER<-unique(fillWithThis[,identifier]) 
  i<-0
  for(i in 1:length(IDENTIFIER)){
    
    #subset fillWithThis by identifier
    tmp2<-fillWithThis[fillWithThis[,identifier] == IDENTIFIER[i],]
    
    #identify toBeFilled row that needs to be filled
    rows<-toBeFilled[,identifier]==as.character(IDENTIFIER[i])
    
    #Inv
    mvnInv<-tmp2[,c('meanInv','varInv','nInv')]
    toBeFilled[rows,c('mean_Inv','var_Inv','n_Inv')] <-mvnInv
    
    #Nat
    mvnNat<-tmp2[,c('meanNat','varNat','nNat')]
    toBeFilled[rows,c('mean_Nat','var_Nat','n_Nat')] <-mvnNat
    
    #unit, qualityMeas, qualityNumSp
    uqq<-tmp2[,c('unit','qualityMeas','qualityNumSp')]
    toBeFilled[rows,c('unit','qualityMeas','qualityNumSp')] <-uqq
  }
  
  return(toBeFilled)
}




### Function to summarize how populated the datatable is ####################################################
SparseSumm.cover<-function(filledDf){
  m.filledDf<-melt(filledDf, id.var=1:4) #melt data
  sparse.fill<-ddply(m.filledDf, ~variable, summarise, #summarize by those new identifier columns
                     numNA = sum(is.na(value)),
                     numNotNA = sum(!is.na(value)))
  return(sparse.fill)
}








