#rmdCode/paperData/script_load_fixNumeric.R
#Make sure data values numeric 
#Currently, values in raw excel files are coded as factors

#A.Fix values using this fxn, created for format where there are invaded and native values
FixVals<-function(df,these){
  i<-0
  for(i in 1:length(these)){
    this<-grep(these[i], colnames(df))
    if(length(this)>1){print("warning")}
    colname<-colnames(df)[this]
    df[,colname]<-as.numeric(as.character(df[,colname]))
  }
  return(df)
}
#For measures, cover
these<-c('InvMean','InvVar','InvN','NatMean','NatVar','NatN') 
measures<-FixVals(df=measures, these=these)
measAgg<-FixVals(df=measAgg, these=these)
cover<-FixVals(df=cover, these=these)
covAgg<-FixVals(df=covAgg, these=these)
#Note: warning messages 'NAs introduced by coercion' are because these data columns had 'AGG' as a placeholder

#B.Fix values using this fxn, created for format where is 1 set of values
FixVals.tr<-function(df,these){
  i<-0
  for(i in 1:length(these)){
    this<-which(colnames(df)==these[i])
    if(length(this)>1){print("warning")}
    colname<-colnames(df)[this]
    df[,colname]<-as.numeric(as.character(df[,colname]))
  }
  return(df)
}
#For traits
these<-c('traitMean','traitVar','traitN')
traits<-FixVals.tr(df=traits, these=these)
these<-c('ta_Mean','ta_Var','ta_N')
traitAgg<-FixVals.tr(df=traitAgg, these=these)
#Note: warning messages 'NAs introduced by coercion' are because these data columns had 'AGG' as a placeholder


