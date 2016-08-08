#script_agg_cover.R
#Aggregate cover

require(plyr) #for ldply
source('CODE/paperData/fxn_1_Calcs.R')
source('CODE/paperData/fxn_2_Agg.R')
source('CODE/paperData/fxn_3_NumOps.R')
source('CODE/paperData/fxn_InsertAgg.R')
source('CODE/paperData/fxn_4_UnitProb.R')

#1. Fix Typos
#none




#2. Identify the operation types and the max number of observations per 1 aggID. Make sure that this matches the Aggregate fxns
unique(covAgg$operation1); unique(covAgg$operation2)
# max 2 operations per 1 aggID

#2b. Isolate problematic aggIDs
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

##operation 2
colmean2<-c('Inv.mean','Nat.mean')
colvar2<-c('Inv.var','Nat.var')
coln2<-c('Inv.n','Nat.n')
colop2='operation2'
colorder2='notes' #just need a placeholder here
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
for (i in 1:length(AGGID)){
  
  # SUBSET 1 AGGID
  sub.Agg<-initAggdata[initAggdata$aggID == AGGID[i],]
  
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
# don't worry about the warning messages - these indicate that there was missing data, NAs, that were turned into Infs




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
source('CODE/paperData/script_agg_coverProblems.R') 
#TASK= do unique aggregation operations
#NEEDS= covAgg, agg.Probs, cover.new
#MAKES= cover.new2



#8. Put the problematic aggregated data back into the original data sheet
#View(cover.new2)
cover<-cover.new2


