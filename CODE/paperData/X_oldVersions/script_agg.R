#paperData_aggregate/script_aggc.R
#Aggregate data for cover, traits, measures from covAgg, etc



### Load Fxns and Libraries ####################################################
library(plyr) #for ldply
source('rmdCode/1_paperData/fxn_1_Calcs.R')
source('rmdCode/1_paperData/fxn_2_Agg.R')
source('rmdCode/1_paperData/fxn_3_NumOps.R')
#source('rmdCode/1_paperData/fxn_4_UnitProb.R')
#source('rmdCode/1_paperData/fxn_InsertAgg.R')




### Aggregate Cover ####################################################

#1. Fix Typos
#none

#2. Identify the operation types and the max number of observations per 1 aggID. Make sure that this matches the Aggregate fxns
unique(covAgg$operation1); unique(covAgg$operation2)
# max 2 operations per 1 aggID

#2b. Isolate problematic aggIDs
#obsID=242.01 - These values represent total cover. Need to aggregate total cover values and then subtract invader cover from total cover to get native cover
agg.Probs<-unique(covAgg[covAgg$obsID==242.01,'aggID'])

#3. Identify the Aggregation fxn parameters based on this particular dataset
##for this dataset
initAggdata<-covAgg
invnat <- TRUE
##operation 1
colmean1<-c('ca_InvMean','ca_NatMean')
colvar1<-c('ca_InvVar_VAR','ca_NatVar_VAR')
coln1<-c('ca_InvN','ca_NatN')
colop1='operation1'
colorder1='notes' #just need a placeholder here
colid1='identifier1'
## operation 2
colmean2<-c('mean')
colvar2<-c('var')
coln2<-c('n')
colop2='operation2'
colorder2='notes' #just need a placeholder here
colid2='identifier2'

#4. Aggregate: Loop through each aggID
agg.result.list<-list()
AGGID<-unique(initAggdata$aggID)
i<-0
for (i in 1:length(AGGID)){
  
  # SUBSET 1 AGGID
  sub.Agg<-initAggdata[initAggdata$aggID == AGGID[i],]
  sub.Agg
  # IDENTIFY NUMOPS
  if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))==0){numops<-1}
  if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))>0){numops<-2}
  
  # NUMOPS == 1
  numops
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
} #THIS ISN'T WORKING


#5. Save results in a dataframe, aggTab, and clean of Infs and NaNs
aggTab<-ldply(agg.result.list,data.frame)
tmp<-aggTab[,!colnames(aggTab) %in% c('.id','aggID','oper')]
tmp[sapply(tmp, is.infinite)]<-NA
tmp[sapply(tmp, is.nan)]<-NA
aggTab[,!colnames(aggTab) %in% c('.id','aggID','oper')]<-tmp

#6. Put the aggregated data back into the original data sheet 
final<-aggTab
df.new<-cover
colvartype<-'covVarType'
colfinals<-c('Inv.mean','Inv.var','Inv.n','Nat.mean','Nat.var','Nat.n')
coldfs<-c('covInvMean','covInvVar_VAR','covInvN','covNatMean','covNatVar_VAR','covNatN')
# Insert new values and update variance type
cover.new<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)
#View(cover.new)

#7. Aggregate the aggProbs aggIDs by hand
agg.Probs # DEAL WITH THIS
source('code/paperData/script_aggProblems.R') # TASK= ; NEEDS= covAgg, agg.Probs, cover.new; MAKES= cover.new3
#View(cover.new3)
cover<-cover.new3



### Aggregate Traits ####################################################

#1. Fix Typos
# Problem1 = need to get rid of space after comma
#traitAgg$operation2order #need to get rid of space after comma
messedup<-grep(", ", as.character(traitAgg[,'operation2order']))
messedup #none
traitAgg[messedup,'operation2order'] #these are all the same, so I can just replace it with the same character string
traitAgg[messedup,'operation2order']<-'N,biomass'

#2. Identify the operation types and the max number of observations per 1 aggID. Make sure that this matches the Aggregate fxns
unique(traitAgg$operation1); unique(traitAgg$operation2)
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
agg.result.list<-list()
agg.unitProbs<-character(0)
AGGID<-unique(initAggdata$aggID)
#which(AGGID == '29.01.1')
i<-0
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
} #THIS ISN'T WORKING


#5. Save results in a dataframe, aggTab
aggTab<-ldply(agg.result.list,data.frame) 
#aggTab


#6. Put the aggregated data back into the original data sheet
final<-aggTab
df.new<-traits
colvartype<-'traitVarType'
colfinals<-c('mean','var','n')
coldfs<-c('traitMean','traitVar_VAR','traitN')
# Insert new values and update variance type
traits.new<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)

#7. Aggregate the agg.Probs and the unitProbs aggIDs by hand
agg.Probs
agg.unitProbs # DEAL WITH THIS
source('code/paperData/script_aggProblems.R') # TASK= ; NEEDS= traitAgg, agg.unitProbs, traits.new; MAKES= traits.new3
#View(traits.new3)
traits<-traits.new3






### Aggregate Measures ####################################################

#1. Fix Typos
#none

#2. Identify the operation types and the max number of observations per 1 aggID. Make sure that this matches the Aggregate fxns
unique(measAgg$operation1)
unique(measAgg$operation2)
unique(measAgg$operation3)
opvec<-unique(measAgg$operation2)
opvec #all the operation types in this dataset
# max 3 operations per 1 aggID

#2b. Isolate problematic aggIDs
agg.Probs<-c(unique(measAgg[traitAgg$obsID %in% c(42.01, 42.02, 42.03, 42.04, 42.05, 42.06, 42.07),'aggID']),
             unique(measAgg[traitAgg$obsID==146.13,'aggID']),
             unique(measAgg[traitAgg$obsID==167.01,'aggID']),
             unique(measAgg[traitAgg$obsID %in% c(286.01, 286.02),'aggID']),
             unique(measAgg[traitAgg$obsID %in% c(461.01, 461.02, 461.03, 461.04, 461.05, 461.06, 
                                                  461.07, 461.08, 461.09, 461.10, 461.11, 461.12,
                                                  461.13, 461.14, 461.15, 461.16),'aggID']),
             unique(measAgg[traitAgg$obsID %in% c(681.01, 681.02, 681.03, 681.04),'aggID']))
agg.Probs

#3. Identify the Aggregation fxn parameters based on this particular dataset
##for this dataset
initAggdata<-measAgg
invnat <- TRUE
##operation 1
colmean1<-c('ma_InvMean','ma_NatMean')
colvar1<-c('ma_InvVar_VAR','ma_NatVar_VAR')
coln1<-c('ma_InvN','ma_NatN')
colop1='operation1'
colorder1='operation1order'
colid1='identifier1'
##operation 2
colmean2<-c('Inv.mean','Nat.mean')
colvar2<-c('Inv.var','Nat.var')
coln2<-c('Inv.n','Nat.n')
colop2='operation2'
colorder2='operation2order'
colid2='identifier2'
##operation 3
colmean2<-c('Inv.mean','Nat.mean')
colvar2<-c('Inv.var','Nat.var')
coln2<-c('Inv.n','Nat.n')
colop3='operation3'
colorder3='operation3order'
colid3='identifier3'

#4. Aggregate: Loop through each aggID
agg.result.list<-list()
agg.unitProbs<-character(0)
agg.numops<-numeric(0)
AGGID<-unique(initAggdata$aggID)
measures[measures$obsID == '15.01' & measures$measCat=='soilmoi','aggID']
which(AGGID == '15.01.4')
i<-0
for (i in 1:length(AGGID)){
  
  # SUBSET 1 AGGID
  sub.Agg<-initAggdata[initAggdata$aggID == AGGID[i],]
  
  unitProblems<-UnitProb(sub.Agg)
  if(unitProblems == 1){
    #record the aggID
    agg.unitProbs<-c(agg.unitProbs,as.character(AGGID[i]))
    }else{
    # IDENTIFY NUMOPS
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))==0 & sum(!is.na(sub.Agg$operation3))==0){numops<-1}
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))>0 & sum(!is.na(sub.Agg$operation3))==0){numops<-2}
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))>0 & sum(!is.na(sub.Agg$operation3))>0){numops<-3}
    agg.numops<-c(agg.numops, numops)
    
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
    
    # NUMOPS == 3
    if(numops==3){ 
      results<-NumOp3(sub.Agg=sub.Agg, 
                      colop1=colop1, colorder1=colorder1, colid1=colid1, 
                      colop2=colop2, colorder2=colorder2, colid2=colid2,  
                      colop3=colop3, colorder3=colorder3, colid3=colid3,  
                      invnat=invnat)
      agg.result.list[[as.character(AGGID[i])]]<-results
    }
  }
}
aggOps<-data.frame(obsID=AGGID, n=agg.numops)

#5. Save results in a dataframe, aggTab
agg.unitProbs #make sure there are none of these
aggTab<-ldply(agg.result.list,data.frame)
tmp<-aggTab[,!colnames(aggTab) %in% c('.id','aggID','oper')]
tmp[sapply(tmp, is.infinite)]<-NA
tmp[sapply(tmp, is.nan)]<-NA
aggTab[,!colnames(aggTab) %in% c('.id','aggID','oper')]<-tmp
#View(aggTab)

#6. Put the aggregated data back into the original data sheet
final<-aggTab
df.new<-measures
colvartype<-'measVarType'
colfinals<-c('Inv.mean','Inv.var','Inv.n','Nat.mean','Nat.var','Nat.n')
coldfs<-c('measInvMean','measInvVar_VAR','measInvN','measNatMean','measNatVar_VAR','measNatN')
# Insert new values and update variance type
measures.new<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)
#View(measures.new)
measures<-measures.new


#what is going on with 15.01 and 58.01 soil moisture?

