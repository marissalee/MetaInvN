#paperData_aggregate/fxn_InsertAgg.R
#Function to insert aggregated values into the main data table and update the variance type associated with the new variance value




### Create a function to insert aggregated data into the main datatable and update the VarType accordingly ####################################################
#df.new is a dataframe, e.g. traits
#final is a dataframe that hold the aggregated data
#colvartype<-'measVarType'
#colfinals<-c('Inv.mean','Inv.var','Inv.n','Nat.mean','Nat.var','Nat.n')
#coldfs<-c('measInvMean','measInvVar','measInvN','measNatMean','measNatVar','measNatN')
#invnat = TRUE/FALSE; TRUE for cover and measure data, FALSE for trait data
InsertAggs<-function(final, df.new, colvartype, colfinals, coldfs, invnat){
  
  # ADD 'VAR' AS A LEVEL TO 'VARTYPE' COLUMN IF IT IS NOT ALREADY PRESENT
  if(sum(levels(df.new[,colvartype]) == 'Var') == 0){
    levels(df.new[,colvartype]) <- c(levels(df.new[,colvartype]), "Var")
  }
  
  
  # LOOP THROUGH EACH AGGID
  AGGID<-unique(final$aggID)
  i<-0
  for (i in 1:length(AGGID)){
    
    # Identify the rows in df.new that match that aggID
    row.num<-which(df.new$aggID == AGGID[i])
    
    # Pull out the new mean, var, n and then insert new mean, var, n into the approp rows in df.new
    df.new[row.num,coldfs] <- final[final$aggID == AGGID[i],colfinals]
    
    # Update the VarType because all aggregated var is now in the form of variance
    if(invnat==TRUE){
      if(sum(!is.na(final[final$aggID == AGGID[i],colfinals[2]])) > 0){
        df.new[row.num,colvartype] <- 'Var' 
      }
    }
    if(invnat==FALSE){
      if(sum(!is.na(final[final$aggID == AGGID[i],colfinals[2]])) > 0){
        df.new[row.num,colvartype] <- 'Var' 
      }
    }
  }
  
  
  # RETURN THE UPDATED DATAFRAME
  return(df.new)
}


