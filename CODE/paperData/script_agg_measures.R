#1_paperData/script_agg_measures.R
#Aggregate measures 

library(plyr) #for ldply
source('CODE/paperData/fxn_1_Calcs.R')
source('CODE/paperData/fxn_2_Agg.R')
source('CODE/paperData/fxn_3_NumOps.R')
source('CODE/paperData/fxn_InsertAgg.R')
source('CODE/paperData/fxn_4_UnitProb.R')

#1. Fix Typos
#none




#2. Identify the operation types and the max number of observations per 1 aggID. Make sure that this matches the Aggregate fxns
#colnames(measAgg)
#unique(measAgg$operation1); unique(measAgg$operation2); unique(measAgg$operation3)
# max 3 operations per 1 aggID

#2b. Isolate problematic aggIDs
agg.Probs<-paste('167.0',1:2,'.1', sep="")




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

#identify unique aggIDs
AGGID1<-unique(initAggdata$aggID)
AGGID<-AGGID1[!AGGID1 %in% agg.Probs]

#make a table of AGGID by operation1type, operation2type for reference
i<-0
result.list<-list()
for(i in 1:length(AGGID)){
  op1.unique<-paste(unique(initAggdata[initAggdata$aggID == AGGID[i],'operation1']), collapse=',')
  op2.unique<-paste(unique(initAggdata[initAggdata$aggID == AGGID[i],'operation2']), collapse=',')
  op3.unique<-paste(unique(initAggdata[initAggdata$aggID == AGGID[i],'operation3']), collapse=',')
  result.list[[i]]<-data.frame(AGGID[i], op1.unique, op2.unique, op3.unique)
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
  
  unitProblems<-UnitProb(sub.Agg)
  if(unitProblems == 1){
    #record the aggID
    agg.unitProbs<-c(agg.unitProbs,as.character(AGGID[i]))
  }else{
    # IDENTIFY NUMOPS
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))==0 & sum(!is.na(sub.Agg$operation3))==0){numops<-1}
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))>0 & sum(!is.na(sub.Agg$operation3))==0){numops<-2}
    if(sum(!is.na(sub.Agg$operation1))>0 & sum(!is.na(sub.Agg$operation2))>0 & sum(!is.na(sub.Agg$operation3))>0){numops<-3}
    
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
# don't worry about the warning messages - these indicate that there was missing data, NAs, that were turned into Infs




#5. Save results in a dataframe, aggTab
aggTab<-ldply(agg.result.list,data.frame)
tmp<-aggTab[,!colnames(aggTab) %in% c('.id','aggID','oper')]
tmp[sapply(tmp, is.infinite)]<-NA
tmp[sapply(tmp, is.nan)]<-NA
aggTab[,!colnames(aggTab) %in% c('.id','aggID','oper')]<-tmp




#6. Put the aggregated data back into the original data sheet
final<-aggTab
df.new<-measures
colvartype<-'measVarType'
colfinals<-c('Inv.mean','Inv.var','Inv.n','Nat.mean','Nat.var','Nat.n')
coldfs<-c('measInvMean','measInvVar_VAR','measInvN','measNatMean','measNatVar_VAR','measNatN')
# Insert new values and update variance type
measures.new<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)
#View(measures.new)




#7a. Look at aggIDs that were flagged for unitProbs.  Remove those that were fixed with convertTo_nh or convertTo_no
agg.unitProbs
criteria<-initAggdata$operation1 %in% c('convertTo_nh','convertTo_no') | initAggdata$operation2 %in% c('convertTo_nh','convertTo_no')
#View(initAggdata[initAggdata$aggID %in% agg.unitProbs & criteria,])
removeThese<-unique(initAggdata[initAggdata$aggID %in% agg.unitProbs & criteria,'aggID'])
agg.unitProbs.fixed<-agg.unitProbs[!agg.unitProbs %in% removeThese]
agg.unitProbs.fixed

#7b. Aggregate the aggProbs aggIDs by hand
source('CODE/paperData/script_agg_measureProblems.R') 
#TASK= do unique aggregation operations
#NEEDS= measAgg, agg.Probs, agg.unitProbs, measure.new
#MAKES= measure.new3




#8. Put the problematic aggregated data back into the original data sheet
#View(measure.new3)
measures<-measures.new3


#what is going on with 15.01 and 58.01 soil moisture?


