#script_stdVar.R
#Script to convert measures of variation (SE, SD, etc) to Var

### Clean varTypes ####################################################
makeNA<-c('3rdquartile', '3rdQuartile','notReported')
df<-list(measures, cover, traits)
df.result<-list()
varTypeCol<-c('measVarType','covVarType','traitVarType')
i<-0
for(i in 1:length(df)){
  #define current dataframe and varType column name
  df.curr<-df[[i]]
  varTypeCol.curr<-varTypeCol[i]
  
  #print initial unique type of variation
  #print(unique(df.curr[,varTypeCol.curr]))
  
  #change varType from a factor to a character
  df.curr[,varTypeCol.curr]<-as.character(df.curr[,varTypeCol.curr])
  
  #replace 'makeNA' with NAs
  subset<-df.curr[,varTypeCol.curr] %in% makeNA
  df.curr[subset,varTypeCol.curr]<-NA
  
  df.result[[i]]<-df.curr
}
measures.c<-df.result[[1]]
cover.c<-df.result[[2]]
traits.c<-df.result[[3]]




### Load Fxns to convert varTypes to VAR ####################################################
source('CODE/paperData/fxn_ConvertVar.R') #for FixVarTypes() and FixVarTypes.Row()
#FixVarTypes() -- coverts df.agg var value to variance (VAR) and adds columns, uses df to index the type of variance (VAR, SD, SE, etc)
#FixVarTypes.Row() -- coverts df var value to variance (VAR) and adds columns




### Convert measures and cover tables to VAR ####################################################

#1. FixVarTypes

#A. measAgg
group.vars<-FixVarTypes(df=measures.c, dfAgg=measAgg, 
                        vartype.colnam='measVarType',
                        varval.colnam=c('ma_InvVar','ma_NatVar'),
                        n.colnam=c('ma_InvN','ma_NatN'),
                        mean.colnam=c('ma_InvMean','ma_NatMean'))
measAgg$ma_InvVar_VAR<-group.vars[,'ma_InvVar']
measAgg$ma_NatVar_VAR<-group.vars[,'ma_NatVar']
#View(measAgg[,c('ma_InvVar','ma_InvVar_VAR')])


#B. covAgg
group.vars<-FixVarTypes(df=cover.c, dfAgg=covAgg,  
                        vartype.colnam='covVarType',
                        varval.colnam=c('ca_InvVar','ca_NatVar'),
                        n.colnam=c('ca_InvN','ca_NatN'),
                        mean.colnam=c('ca_InvMean','ca_NatMean'))
covAgg$ca_InvVar_VAR<-group.vars[,'ca_InvVar']
covAgg$ca_NatVar_VAR<-group.vars[,'ca_NatVar']
#View(covAgg[,c('ca_InvVar','ca_InvVar_VAR')]) #looks good

#C. traitAgg
tmp<-FixVarTypes(df=traits.c, dfAgg=traitAgg, 
                 vartype.colnam='traitVarType',
                 varval.colnam='ta_Var',
                 n.colnam='ta_N',
                 mean.colnam='ta_Mean')
traitAgg$ta_Var_VAR<-tmp
#View(traitAgg[,c('ta_Var','ta_Var_VAR')])


#2. FixVarTypes.Row

#A. measures
tmp<-FixVarTypes.Row(df=measures.c,
                     vartype.colnam='measVarType', 
                     varval.colnam=c('measInvVar','measNatVar'),
                     n.colnam=c('measInvN','measNatN'),
                     mean.colnam=c('measInvMean','measNatMean'))
measures.c$measInvVar_VAR<-tmp[,'measInvVar']
measures.c$measNatVar_VAR<-tmp[,'measNatVar']
#View(measures.c[,c('measVarType','measInvVar','measInvVar_VAR')]) #more missing here than I expected

#B. cover
tmp<-FixVarTypes.Row(df=cover.c,
                     vartype.colnam='covVarType', 
                     varval.colnam=c('covInvVar','covNatVar'),
                     n.colnam=c('covInvN','covNatN'),
                     mean.colnam=c('covInvMean','covNatMean'))
cover.c$covInvVar_VAR<-tmp[,'covInvVar']
cover.c$covNatVar_VAR<-tmp[,'covNatVar']
#View(cover.c[,c('covVarType','covInvVar','covInvVar_VAR')])

#C. traits
curr.vars<-FixVarTypes.Row(df=traits.c,
                           vartype.colnam='traitVarType', 
                           varval.colnam='traitVar',
                           n.colnam='traitN')
traits.c$traitVar_VAR<-curr.vars
#View(traits.c[,c('traitVarType','traitVar','traitVar_VAR')])


## Update the main dataframes
measures<-measures.c
cover<-cover.c
traits<-traits.c


