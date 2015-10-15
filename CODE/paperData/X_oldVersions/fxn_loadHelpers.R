#fxn_loadHelpers.R
#Fxns to help clean data immediate after it has been loaded


# ### FXN to change 'NA' to NA ####################################################
# FixNAs<-function(dat){
#   dat2 <- as.matrix(dat) # convert to matrix 
#   y <- which(dat == 'NA') # get index of 'NA' values 
#   dat2[y] <- NA # replace with NA
#   dat.fixed<-as.data.frame(dat2)
#   
#   return(dat.fixed)
# }


# 
# 
# ### FXN to make value cols 'as.numeric' for measures and cover ####################################################
# FixVals<-function(df,these){
#   i<-0
#   for(i in 1:length(these)){
#     this<-grep(these[i], colnames(df))
#     if(length(this)>1){print("warning")}
#     colname<-colnames(df)[this]
#     df[,colname]<-as.numeric(as.character(df[,colname]))
#   }
#   return(df)
# }
# 
# 
# 
# 
# ### FXN to make value cols 'as.numeric' for traits ####################################################
# FixVals.tr<-function(df,these){
#   i<-0
#   for(i in 1:length(these)){
#     this<-which(colnames(df)==these[i])
#     if(length(this)>1){print("warning")}
#     colname<-colnames(df)[this]
#     df[,colname]<-as.numeric(as.character(df[,colname]))
#   }
#   return(df)
# }
# 



### FXN to change factor columns to character columns ####################################################
FactorsToChar<-function(df){
  tmp <- sapply(df, is.factor)
  df[tmp] <- lapply(df[,tmp], as.character)
  return(df)
}

