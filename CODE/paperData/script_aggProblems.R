#paperData_aggregate/script_aggProblems.R
#Manually aggregate aggIDs that have mis-matched units



#Decide how to deal with these aggIDs
traitAgg[traitAgg$aggID %in% agg.unitProbs,]
#Will aggregate in 3 batches based on the paperID


### obsID == 29.01 and 29.02 ####################################################
probAggIDs<-agg.unitProbs[grepl('29', agg.unitProbs)]
probAggIDs

# Pull out just these aggIDs from the original dataset
storedrows<-numeric(0)
for(i in 1:length(probAggIDs)){
  rows<-traitAgg[traitAgg$aggID == probAggIDs[i],]
  storedrows<-rbind(storedrows,rows)
} #isolate problem datasets from the main dataframe
dfprob<-storedrows

# Identify the Aggregation fxn based on this particular dataset - Make a custom 'Aggregate' fxn
AggFXN<-function(df1){
  
  # OPERATION 1: Add root and shoot biomass
  group.meanBiom1<-df1[df1$identifier2=="biomass",'ta_Mean'] # Define groupent value
  group.varBiom1<-df1[df1$identifier2=="biomass",'ta_Var_VAR']
  group.nsBiom1<-df1[df1$identifier2=="biomass",'ta_N']
  tmp<-CalcSum(group.means=group.meanBiom1, group.vars=group.varBiom1, group.ns=group.nsBiom1)
  group.meanBiom<-tmp$global.mean
  group.varBiom<-tmp$global.var
  group.nsBiom<-tmp$global.n
  
  # PREP OPERATION 2: Convert mgN to gN
  group.meanN1<-df1[df1$identifier2=="N",'ta_Mean'] # Define groupent value
  group.varN1<-df1[df1$identifier2=="N",'ta_Var_VAR']
  group.nsN1<-df1[df1$identifier2=="N",'ta_N']
  group.meanN<-group.meanN1 * (1/1000) # mgN /1000 = gN
  group.varN<-group.varN1 * (1/1000)^2 #remember that Var(aX) = a^2 * Var(X)
  
  # OPERATION 2: Calc %N
  global<-CalcPercent(group.means1=group.meanN, group.means2=group.meanBiom, 
              group.vars1=group.varN, group.vars2=group.varBiom, 
              group.ns1=group.nsN1, group.ns2=group.nsBiom)
  
  #SUMMARIZE global VALUES
  results<-data.frame(aggID=unique(df1[,'aggID']), mean=global$global.mean, var=global$global.var, n=global$global.n)
  
  #RETURN RESULTS
  return(results)
}

# Aggregate: Loop through each aggID
AGGID<-unique(dfprob$aggID)
agg.result.list<-list()
i<-0
for (i in 1:length(AGGID)){
  df1<-dfprob[dfprob$aggID == AGGID[i],] # SUBSET ROWS TO AGGREGATE
  results<-AggFXN(df1) # DO CUSTOM AGGREGATION
  agg.result.list[[as.character(AGGID[i])]]<-results # SAVE RESULTS
}

# Save results in a dataframe, aggTab
aggTab.prob<-ldply(agg.result.list,data.frame)
aggTab.prob

# Put the aggregated data back into the original data sheet
final<-aggTab.prob
df.new<-traits.new # has already been updated with the non-problematic traitAgg data
colvartype<-'traitVarType'
colfinals<-c('mean','var','n')
coldfs<-c('traitMean','traitVar_VAR','traitN')
invnat<-FALSE
# Insert new values and update variance type
traits.new1<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)
#View(traits.new1)




### obsID == 236.01 ####################################################
probAggIDs<-agg.unitProbs[grepl('236.01', agg.unitProbs)]

# Pull out just these aggIDs from the original dataset
storedrows<-numeric(0)
for(i in 1:length(probAggIDs)){
  rows<-traitAgg[traitAgg$aggID == probAggIDs[i],]
  storedrows<-rbind(storedrows,rows)
} #isolate problem datasets from the main dataframe
dfprob<-storedrows

# Identify the Aggregation fxn based on this particular dataset - Make a custom 'Aggregate' fxn
AggFXN<-function(df1){
  
  # PREP OPERATION 1: Convert N value from mg to g
  group.meanN1<-df1[df1$identifier1=="N",'ta_Mean'] # Define groupent value
  group.meanN<-group.meanN1 * (1/1000) # mgN /1000 = gN
  group.varN1<-df1[df1$identifier1=="N",'ta_Var_VAR']
  group.varN<-group.varN1 * (1/1000)^2 #remember that Var(aX) = a^2 * Var(X)
  # DO OPERATION 1: Divide gN by gbiomass and x100 to get N%
  group.meanBiom<-df1[df1$identifier1=='biomass','ta_Mean'] # Define groupent value
  group.nsN<-df1[df1$identifier1=='N','ta_N']
  group.nsBiom<-df1[df1$identifier1=='biomass','ta_N']
  group.varBiom<-df1[df1$identifier1=="biomass",'ta_Var_VAR']
  global<-CalcPercent(group.means1=group.meanN, group.means2=group.meanBiom, 
                      group.vars1=group.varN, group.vars2=group.varBiom, 
                      group.ns1=group.nsN, group.ns2=group.nsBiom)
  
  #SUMMARIZE global VALUES
  results<-data.frame(aggID=unique(df1[,'aggID']), mean=global$global.mean, var=global$global.var, n=global$global.n)
  
  #RETURN RESULTS
  return(results)
}

# Aggregate: Loop through each aggID
AGGID<-unique(dfprob$aggID)
agg.result.list<-list()
i<-0
for (i in 1:length(AGGID)){
  df1<-dfprob[dfprob$aggID == AGGID[i],] # SUBSET ROWS TO AGGREGATE
  results<-AggFXN(df1) # DO CUSTOM AGGREGATION
  agg.result.list[[as.character(AGGID[i])]]<-results # SAVE RESULTS
}

# Save results in a dataframe, aggTab
aggTab.prob<-ldply(agg.result.list,data.frame)
#aggTab.prob

# Put the aggregated data back into the original data sheet
final<-aggTab.prob
df.new<-traits.new1 # has already been updated with the non-problematic traitAgg data
colvartype<-'traitVarType'
colfinals<-c('mean','var','n')
coldfs<-c('traitMean','traitVar_VAR','traitN')
invnat<-FALSE
# Insert new values and update variance type
traits.new2<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)
#View(traits.new1)





### obsID == 716.01 ####################################################
probAggIDs<-agg.unitProbs[grepl('716.01', agg.unitProbs)]

# Identify the operation types and the max number of observations per 1 aggID. Make sure that this matches the Aggregate fxns
storedrows<-numeric(0)
for(i in 1:length(probAggIDs)){
  rows<-traitAgg[traitAgg$aggID == probAggIDs[i],]
  storedrows<-rbind(storedrows,rows)
  } #isolate problem datasets from the main dataframe
dfprob<-storedrows 

# Identify the Aggregation fxn based on this particular dataset - Make a custom 'Aggregate' fxn
AggFXN<-function(df1){
  
  # DO OPERATION 1 OF 2: Convert N value from a ratio (mgN/gN) to N %
  # Define groupent value
  group.mean<-df1[df1$identifier2=="N",'ta_Mean'] 
  # Update groupent value
  group.meanA <- group.mean * 0.001 #1. make the units the same in the numerator and denominator; group.mean1 (gN/g) 
  group.meanB <- group.meanA * 100 #2. multiply by 100 to convert to a percentage
  
  # DO OPERATION 2 OF 2: Divide C% by N% to get C:N ratio
  # Define groupent value
  group.means1<-df1[df1$identifier2=='C','ta_Mean']
  group.means2<-group.meanB
  group.ns1<-df1[df1$identifier2=='C','ta_N']
  group.ns2<-df1[df1$identifier2=='N','ta_N']
  # Update groupent value
  global.mean<-group.means1/group.means2
  global.var<-'NA' # NA because var not reported for this set of aggIDs
  global.n<-sum(c(group.ns1,group.ns2))
  
  #SUMMARIZE global VALUES
  results<-data.frame(aggID=unique(df1[,'aggID']), mean=global.mean, var=global.var, n=global.n)
  
  #RETURN RESULTS
  return(results)
}

# Aggregate: Loop through each aggID
AGGID<-unique(dfprob$aggID)
agg.result.list<-list()
i<-0
for (i in 1:length(AGGID)){
  df1<-dfprob[dfprob$aggID == AGGID[i],] # SUBSET ROWS TO AGGREGATE
  results<-AggFXN(df1) # DO CUSTOM AGGREGATION
  agg.result.list[[as.character(AGGID[i])]]<-results # SAVE RESULTS
}

# Save results in a dataframe, aggTab
aggTab.prob<-ldply(agg.result.list,data.frame)
#aggTab.prob

# Put the aggregated data back into the original data sheet
final<-aggTab.prob
df.new<-traits.new2 # has already been updated with the non-problematic traitAgg data
colvartype<-'traitVarType'
colfinals<-c('mean','var','n')
coldfs<-c('traitMean','traitVar_VAR','traitN')
invnat<-FALSE
# Insert new values and update variance type
traits.new3<-InsertAggs(final=final, df.new=df.new, colvartype=colvartype, colfinals=colfinals, coldfs=coldfs, invnat=invnat)
#View(traits.new3)




