#1_paperData/script_agg_traits.R
#Aggregate Traits

library(plyr) #for ldply
source('rmdCode/paperData/fxn_1_Calcs.R')
source('rmdCode/paperData/fxn_2_Agg.R')
source('rmdCode/paperData/fxn_3_NumOps.R')
source('rmdCode/paperData/fxn_InsertAgg.R')
source('rmdCode/paperData/fxn_4_UnitProb.R')

#1. Fix Typos
# Problem1 = need to get rid of space after comma
#traitAgg$operation2order #need to get rid of space after comma
messedup<-grep(", ", as.character(traitAgg[,'operation2order']))
messedup #none
traitAgg[messedup,'operation2order'] #these are all the same, so I can just replace it with the same character string
traitAgg[messedup,'operation2order']<-'N,biomass'




#2. Identify the operation types and the max number of observations per 1 aggID. Make sure that this matches the Aggregate fxns
colnames(traitAgg); unique(traitAgg$operation1); unique(traitAgg$operation2)
# max 2 operations per 1 aggID

#2b. Isolate problematic aggIDs
agg.Probs<-c(unique(traitAgg[traitAgg$obsID==29.01,'aggID']),
             unique(traitAgg[traitAgg$obsID==29.02,'aggID']),
             unique(traitAgg[traitAgg$obsID==353.01,'aggID']),
             unique(traitAgg[traitAgg$obsID==716.01,'aggID']))
agg.Probs




#3. Identify the Aggregation fxn parameters based on this particular dataset
## for this dataset
initAggdata<-traitAgg
invnat <- FALSE

## operation 1
colmean1<-c('ta_Mean')
colvar1<-c('ta_Var_VAR')
coln1<-c('ta_N')
colop1='operation1'
colorder1='operation1order'
colid1='identifier1'

## operation 2
colmean2<-c('mean')
colvar2<-c('var')
coln2<-c('n')
colop2='operation2'
colorder2='operation2order'
colid2='identifier2'




#4. Aggregate: Loop through each aggID

#identify unique aggIDs
AGGID1<-unique(initAggdata$aggID)
AGGID<-AGGID1[!AGGID1 %in% agg.Probs]

#make a table of AGGID by operation1type, operation2type for reference
i<-0
result.list<-list()
for(i in 1:length(AGGID)){
  op1.unique<-paste(unique(initAggdata[initAggdata$aggID == AGGID[i],'operation1']), collapse=',')
  op2.unique<-paste(unique(initAggdata[initAggdata$aggID == AGGID[i],'operation2']), collapse=',')
  result.list[[i]]<-data.frame(AGGID[i], op1.unique, op2.unique)
}
reftab<-ldply(result.list)
#View(reftab)

#loop through each aggID and do the operations to aggregate
i<-0
agg.result.list<-list()
agg.unitProbs<-character(0)
for (i in 1:length(AGGID)){
  
  # SUBSET 1 AGGID
  sub.Agg<-initAggdata[initAggdata$aggID == AGGID[i],]
  
  # IDENTIFY UNIT PROBLEMS
  unitProblems<-UnitProb(sub.Agg)
  
  if(unitProblems == 1){
    #record the aggID
    agg.unitProbs<-c(agg.unitProbs,as.character(AGGID[i]))
  }else{
    # IDENTIFY NUMOPS
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))==0){numops<-1}
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))>0){numops<-2}
    
    # NUMOPS == 1
    if(numops==1){
      results<-NumOp1(sub.Agg=sub.Agg, 
                      colop1=colop1, colorder1=colorder1, colid1=colid1, 
                      invnat=invnat)
      agg.result.list[[as.character(AGGID[i])]]<-results
    }
    
    # NUMOPS == 2
    if(numops==2){ 
      results<-NumOp2(sub.Agg=sub.Agg, 
                      colop1=colop1, colorder1=colorder1, colid1=colid1, 
                      colop2=colop2, colorder2=colorder2, colid2=colid2,  
                      invnat=invnat)
      agg.result.list[[as.character(AGGID[i])]]<-results 
    }
  }
} 

#5. Save results in a dataframe, aggTab
aggTab<-ldply(agg.result.list,data.frame) 



#6. Put the aggregated data back into the original data sheet
final<-aggTab
df.new<-traits
colvartype<-'traitVarType'
colfinals<-c('mean','var','n')
coldfs<-c('traitMean','traitVar_VAR','traitN')
# Insert new values and update variance type
traits.new<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)



#7. Aggregate the aggProbs aggIDs by hand
source('rmdCode/paperData/script_agg_traitProblems.R') 
#TASK= do unique aggregation operations
#NEEDS= traitAgg, agg.unitProbs, traits.new
#MAKES= traits.new4



#8. Put the problematic aggregated data back into the original data sheet
#View(traits.new4)
traits<-traits.new4




