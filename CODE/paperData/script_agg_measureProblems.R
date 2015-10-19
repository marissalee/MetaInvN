#script_agg_measureProblems.R
#Manually aggregate problematic aggIDs


#Decide how to deal with these aggIDs
agg.Probs<-c(agg.Probs,agg.unitProbs.fixed)
agg.Probs
#measAgg[measAgg$aggID %in% agg.Probs,]



# Define fxn to pull out just these aggIDs from the original dataset
PullFxn<-function(probAggIDs, measAgg){
  i<-0
  storedrows<-numeric(0)
  for(i in 1:length(probAggIDs)){
    rows<-measAgg[measAgg$aggID == probAggIDs[i],]
    storedrows<-rbind(storedrows,rows)
  } #isolate problem datasets from the main dataframe
  
  return(storedrows)
}

# Define Fxn to Aggregate using the custom Fxn and put the aggregated data back into the original datasheet
FillFxn<-function(agg.result.list, dfTBUpdated){
  
  # Save results in a dataframe, aggTab
  aggTab.prob<-ldply(agg.result.list,data.frame)
  
  # Put the aggregated data back into the original data sheet
  dfUpdated<-InsertAggs(final=aggTab.prob, 
                        df.new=dfTBUpdated, 
                        colvartype='measVarType', 
                        colfinals=c('Inv.mean','Inv.var','Inv.n', 'Nat.mean','Nat.var','Nat.n'), 
                        coldfs=c('measInvMean','measInvVar_VAR','measInvN','measNatMean','measNatVar_VAR','measNatN'), 
                        invnat=TRUE)
  
  return(dfUpdated)
}

# Define GLOBAL parameters for AggFXNs
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

# Define columns to check that everything went ok
checkcols<-c('aggID','measInvMean','measInvVar_VAR','measInvN','measNatMean','measNatVar_VAR','measNatN')


### aggID == 167.XX.1 ####################################################
probAggIDs<-paste('167.0',1:2,'.1', sep="")
probAggIDs
dfprob<-PullFxn(probAggIDs, measAgg)
dfprob

#Custom AggFXN
#Average across 4 sites, this is a canopy openness measures that needs to be converted into plant cover (100-canopyopen = plant cov)
AggFXN<-function(df1){
  
  # OPERATION 1: Convert canopy openness measures to total plant cover
  group.meanOpen<-df1[,colmean1] # Define groupent value
  group.varOpen<-df1[,colvar1]
  group.nsOpen<-df1[,coln1]
  group.mean<-100-group.meanOpen
  group.var<-group.varOpen
  group.ns<-group.nsOpen
  
  # OPERATION 2: Average
  global.Invs<-CalcMean(group.mean[,1], group.var[,1], group.ns[,1])
  global.Nats<-CalcMean(group.mean[,2], group.var[,2], group.ns[,2])
  
  # SAVE AND RETURN RESULTS
  results<-data.frame(aggID=unique(df1[,'aggID']), 
                      Inv.mean=global.Invs$global.mean, Inv.var=global.Invs$global.var, Inv.n=global.Invs$global.n,
                      Nat.mean=global.Nats$global.mean, Nat.var=global.Nats$global.var, Nat.n=global.Nats$global.n)
  return(results)
}

#Aggregate using the custom Fxn
AGGID<-unique(dfprob$aggID)
agg.result.list<-list()
i<-0
for (i in 1:length(AGGID)){
  df1<-dfprob[dfprob$aggID == AGGID[i],] # SUBSET ROWS TO AGGREGATE
  results<-AggFXN(df1) # DO CUSTOM AGGREGATION
  agg.result.list[[as.character(AGGID[i])]]<-results # SAVE RESULTS
}

#Put the aggregated data back into the original datasheet
measures.new1<-FillFxn(agg.result.list, dfTBUpdated=measures.new)
measures.new1[measures.new1$aggID %in% probAggIDs,checkcols]




### aggID == 7.01.3 ####################################################
probAggIDs<-'7.01.3'
dfprob<-PullFxn(probAggIDs, measAgg)
dfprob

#Custom AggFXN
# First, average across soil depths and convert C from g/m2 to mg/m2
# Second, divide C by N
AggFXN<-function(df1){
  
  # OPERATION 1: Average across soil depths and make C g/m2 into mg/m2
  
  # Define parameters
  oper<-unique(df1[,colop1])[!is.na(unique(df1[,colop1]))] #operation type
  ROWGROUP<-unique(df1[,colid2]) #identity of rows to be aggregated; notice that this is identifier2, not 1
  op.params<-list(colmean=colmean1, #column name index.. should be 1s since operation 1
                  colvar=colvar1,
                  coln=coln1,
                  colorder=colorder1,
                  colid=colid1)
  
  # Aggregate by ROWGROUP
  C.rows<-df1[df1[,colid2]=="C",] #subset the group of rows to be aggregated
  C.rows[,c('ma_InvMean', 'ma_NatMean')]<-C.rows[,c('ma_InvMean', 'ma_NatMean')] * 1000 #convert C g to C mg
  N.rows<-df1[df1[,colid2]=="N",]
  resultC<-Aggregate(oper=oper,dfagg=C.rows,op.params=op.params,invnat=invnat)
  resultN<-Aggregate(oper=oper,dfagg=N.rows,op.params=op.params,invnat=invnat)
  ROWGROUP.result.list<-list(resultC, resultN)
  names(ROWGROUP.result.list)<-c('C','N')
  op1results<-ldply(ROWGROUP.result.list, data.frame) #change it from a list into a data.frame
  
  # DO OPERATION 2 OF 2: Divide C by N
  
  # Define parameters
  oper<-unique(df1[,colop2])[!is.na(unique(df1[,colop2]))] #operation type
  op1results$oper2<-rep(oper, dim(op1results)[1]) #1) add the operation type to op1results
  op1results$op2order<-rep(unique(df1[,colorder2]), dim(op1results)[1]) #2) add the operation order to op1results
  op.params<-list(colmean=colmean2, #column name index.. should be 2s since operation 2
                  colvar=colvar2,
                  coln=coln2,
                  colorder='op2order', #update the colorder
                  colid='.id') #update the colid
  
  # Aggregate
  op1results
  op.params
  results<-Aggregate(oper=oper,
                     dfagg=op1results,
                     op.params=op.params,
                     invnat=TRUE)
  return(results)
}

#Aggregate using the custom Fxn
AGGID<-unique(dfprob$aggID)
agg.result.list<-list()
i<-0
for (i in 1:length(AGGID)){
  df1<-dfprob[dfprob$aggID == AGGID[i],] # SUBSET ROWS TO AGGREGATE
  results<-AggFXN(df1) # DO CUSTOM AGGREGATION
  agg.result.list[[as.character(AGGID[i])]]<-results # SAVE RESULTS
}

#Put the aggregated data back into the original datasheet
measures.new2<-FillFxn(agg.result.list, dfTBUpdated=measures.new1)
measures.new2[measures.new2$aggID %in% probAggIDs,checkcols]



### aggID == 710.01.7 ####################################################
probAggIDs<-agg.Probs[grepl('710', agg.Probs)]
probAggIDs
dfprob<-PullFxn(probAggIDs, measAgg)
dfprob

#Custom AggFXN - use the fxn that was created for 7.01.3

#Aggregate using the custom Fxn
AGGID<-unique(dfprob$aggID)
agg.result.list<-list()
i<-0
for (i in 1:length(AGGID)){
  df1<-dfprob[dfprob$aggID == AGGID[i],] # SUBSET ROWS TO AGGREGATE
  results<-AggFXN(df1) # DO CUSTOM AGGREGATION
  agg.result.list[[as.character(AGGID[i])]]<-results # SAVE RESULTS
}

#Put the aggregated data back into the original datasheet
measures.new3<-FillFxn(agg.result.list, dfTBUpdated=measures.new2)
measures.new3[measures.new3$aggID %in% probAggIDs,checkcols]







