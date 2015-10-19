#script_agg_coverProblems.R
#Manually aggregate problematic aggIDs


#Decide how to deal with these aggIDs
agg.Probs
covAgg[covAgg$aggID %in% agg.Probs,]
#deal with them separately


### aggID == 242.01.1 ####################################################
probAggIDs<-agg.Probs[grepl('242.01.1', agg.Probs)]

# Pull out just these aggIDs from the original dataset
storedrows<-numeric(0)
for(i in 1:length(probAggIDs)){
  rows<-covAgg[covAgg$aggID == probAggIDs[i],]
  storedrows<-rbind(storedrows,rows)
} #isolate problem datasets from the main dataframe
dfprob<-storedrows

# Operation 1: Average to get invasive sp cover
colmean1<-c('ca_InvMean','ca_NatMean')
colvar1<-c('ca_InvVar_VAR','ca_NatVar_VAR')
coln1<-c('ca_InvN','ca_NatN')
colop1='operation1'
colorder1='notes' #just need a placeholder here
colid1='identifier1'
results<-NumOp1(sub.Agg=dfprob, colop1=colop1, colorder1=colorder1, colid1=colid1, invnat=TRUE)

# Put the aggregated data back into the original data sheet
df.new<-cover.new # has already been updated with the non-problematic covAgg data
cover.new1<-InsertAggs(final=results, 
                       df.new=df.new, 
                       colvartype='covVarType', 
                       colfinals=c('Inv.mean','Inv.var','Inv.n', 'Nat.mean','Nat.var','Nat.n'), 
                       coldfs=c('covInvMean','covInvVar_VAR','covInvN','covNatMean','covNatVar_VAR','covNatN'), 
                       invnat=TRUE)
#View(cover.new1)



### aggID == 242.01.2 ####################################################
probAggIDs<-agg.Probs[grepl('242.01.2', agg.Probs)]

# Pull out just these aggIDs from the original dataset
storedrows<-numeric(0)
for(i in 1:length(probAggIDs)){
  rows<-covAgg[covAgg$aggID == probAggIDs[i],]
  storedrows<-rbind(storedrows,rows)
} #isolate problem datasets from the main dataframe
dfprob<-storedrows

# Operation 1: Average to get total sp cover
colmean1<-c('ca_InvMean','ca_NatMean')
colvar1<-c('ca_InvVar_VAR','ca_NatVar_VAR')
coln1<-c('ca_InvN','ca_NatN')
colop1='operation1'
colorder1='notes' #just need a placeholder here
colid1='identifier1'
results<-NumOp1(sub.Agg=dfprob, colop1=colop1, colorder1=colorder1, colid1=colid1, invnat=TRUE)

# Operation 2: Subtract aggID=242.01.1 value from Operation1 results to get native sp cover
invCovAggID<-agg.Probs[grepl('242.01.1', agg.Probs)]
invCovRow<-cover.new1[cover.new1$aggID == invCovAggID,]

#identify current values
group.meansTot<-results[,c('Inv.mean','Nat.mean')]
group.meansInv<-invCovRow[,c('covInvMean','covNatMean')]
group.varsTot<-results[,c('Inv.var','Nat.var')]
group.varsInv<-invCovRow[,c('covInvVar_VAR', 'covNatVar_VAR')]
group.nsTot<-results[,c('Inv.n','Nat.n')]
group.nsInv<-invCovRow[,c('covInvN','covNatN')]

#Global N
global.n<-group.nsTot
#Global Mean
global.mean<-group.meansTot-group.meansInv
#Global Var
global.var <-group.varsTot+group.varsInv
results<-data.frame(global.mean, global.var, global.n)

# Put the aggregated data back into the original data sheet
df.new<-cover.new1 # has already been updated with the non-problematic covAgg data
cover.new2<-InsertAggs(final=results, 
                       df.new=df.new, 
                       colvartype='covVarType', 
                       colfinals=c('Inv.mean','Inv.var','Inv.n', 'Nat.mean','Nat.var','Nat.n'), 
                       coldfs=c('covInvMean','covInvVar_VAR','covInvN','covNatMean','covNatVar_VAR','covNatN'), 
                       invnat=TRUE)
#View(cover.new2)






