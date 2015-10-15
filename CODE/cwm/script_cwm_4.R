#synthesis/script_c4.R
#Calculate the community-weighted mean for plant traits in invaded and native areas

### Load libraries and fxns ####################################################
library(plyr)
library(reshape2)



### Make each row a unique obsID - Paper-reported CWMs ####################################################
r.cwm<-measures[measures$measCat == 'cn' | measures$measCat == 'percN' | measures$measCat == 'littercn' | measures$measCat == 'litterpercN',]
#View(r.cwm)

#need to make this parallel to the cwm.list dataframes
r.cwm.reshape<-r.cwm #ok
#View(r.cwm.reshape)




### Merge paper-reported and calculated CWMs ####################################################
cwm.r.list<-list()
TRAITS<-names(cwm.list)
i<-0
for(i in 1:length(TRAITS)){
  
  # Calculated CWMs - Pull out the right dataframe and make each row a unique obsID ####################################################
  df1<-data.frame(cwm.list[[i]],stringsAsFactors = FALSE )
  df<-dcast(df1, obsID+var+nTr+unit+relabund_note+invadedArea_invasiveSp+nativeArea_invasiveSp+invadedArea_nativeSp+nativeArea_nativeSp~valueName)
  df$cwmCalc<-rep('calculated', dim(df)[1]) #Add an empty column to indicate whether the data is reported or calculated
  
  # Reported CWMs - Pull out the r.cwm rows for this trait ####################################################
  rows<-r.cwm.reshape$measCat == TRAITS[[i]]
  r.cwm.sub<-r.cwm.reshape[rows,c('obsID','inv_mean_std','nat_mean_std','inv_var_std','nat_var_std','unit_std')]
  
  #Identify the obsID rows of df (calculated cwm) that need to be replaced with reported cwm data
  temp<-r.cwm.sub$obsID %in% df$obsID
  obsIDpresent<-sum(temp == TRUE)
  obsIDnotPresent<-sum(temp == FALSE)
  
  #if the obsID is already present in the data set
  if(obsIDpresent>0){
    
    #Identify the correct obsID rows
    row.dfin<-df$obsID %in% r.cwm.sub[temp, 'obsID']
    
    #Put NAs over everything except the InvSpInvArea data (because that was not reported)
    keepCols<-grepl("_InvSpInvArea",colnames(df))
    keepCols[which(colnames(df)=='obsID')]<-TRUE
    df[row.dfin,!keepCols]<-NA 
    
    #Fill in the reported values
    df[row.dfin,'mean_InvArea'] <- r.cwm.sub[temp,'inv_mean_std']
    df[row.dfin,'mean_NatArea'] <- r.cwm.sub[temp,'nat_mean_std']
    r.cwm.sub[temp, 'inv_var_std']<-as.numeric(r.cwm.sub[temp, 'inv_var_std'])
    r.cwm.sub[temp, 'nat_var_std']<-as.numeric(r.cwm.sub[temp, 'nat_var_std'])
    df[row.dfin,'var'] <- apply(r.cwm.sub[temp, c('inv_var_std','nat_var_std')], 1, mean)
    df[row.dfin,'unit'] <- as.character(r.cwm.sub[temp,'unit_std'])  
    
    #Indicate that the row's info was reported
    df[row.dfin,'cwmCalc']<-'reported'
  }
  #if the obsID is not already present in the dataset
  if(obsIDnotPresent>0){
    
    #Create empty rows of df
    nrows<-length(r.cwm.sub$obsID[!temp]) # number of rows that need to be added
    df.empty<-df[1:nrows,] # use the existing df and pull out the number rows that are needed
    row.names(df.empty)<-r.cwm.sub[!temp,'obsID']
    df.empty[1:nrows,]<-NA # replace rows with NAs
    
    #Fill empty rows of df
    df.empty$obsID<-r.cwm.sub[!temp,'obsID']
    df.empty$mean_InvArea<-r.cwm.sub[!temp,'inv_mean_std']
    df.empty$mean_NatArea <- r.cwm.sub[!temp,'nat_mean_std']
    r.cwm.sub[!temp, 'inv_var_std']<-as.numeric(r.cwm.sub[!temp, 'inv_var_std'])
    r.cwm.sub[!temp, 'nat_var_std']<-as.numeric(r.cwm.sub[!temp, 'nat_var_std'])
    df.empty$var<-apply(r.cwm.sub[!temp, c('inv_var_std','nat_var_std')], 1, mean)
    df.empty$unit<-as.character(r.cwm.sub[!temp,'unit_std'])
    df.empty$cwmCalc<-'reported'
    
    df[sapply(df, is.factor)] <- lapply(df[sapply(df, is.factor)], as.character)
    df.empty[sapply(df.empty, is.factor)] <- lapply(df.empty[sapply(df.empty, is.factor)], as.character)
    
    #Add new rows to df
    df<-rbind(df,df.empty)
  }
  #Save the whole trait dataframe into a list
  cwm.r.list[[as.character(TRAITS[i])]]<-df
}
#str(cwm.r.list)

