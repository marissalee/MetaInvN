#FixUnits.R
#Function to standardize units for each measure. Adds columns to the main dataframe




FixUnits<-function(df, Catcol, meancol, varcol, unitcol, unitIndex, invnat){
  
  # 1. Create extra columns in the df to hold the new values
  if(invnat==TRUE){
    emptylength<-dim(df)[1]
    df$stdmeanInv<-rep(NA,emptylength)
    df$stdmeanNat<-rep(NA,emptylength)
    df$stdvarInv<-rep(NA,emptylength)
    df$stdvarNat<-rep(NA,emptylength)
    df$stdunit<-rep(NA,emptylength)
  }
  if(invnat==FALSE){
    emptylength<-dim(df)[1]
    df$stdmean<-rep(NA,emptylength)
    df$stdvar<-rep(NA,emptylength)
    df$stdunit<-rep(NA,emptylength)
  }
  
  # 2. Loop through each measCat
  CAT<-unique(unitIndex[,Catcol]) #
  i<-0
  for(i in 1:length(CAT)){
    
    # 2A. Identify all the rows in unitIndex that have this CAT and the c.unit
    INDEXROWS<-which(unitIndex[,Catcol]==CAT[i])
    c.unit<-unique(unitIndex[INDEXROWS,'c.unit'])
    
    # 2B. Identify all the rows in df that have this CAT and loop through each CATROW
    CATROWS<-which(df[,Catcol]==CAT[i])
    k<-0
    for(k in 1:length(CATROWS)){
      
      # 2B1. Identify the current value and the current unit
      curr.means<-df[CATROWS[k],meancol]
      curr.vars<-df[CATROWS[k],varcol] #var in the form 'variance'
      curr.unit<-as.character(df[CATROWS[k],unitcol])
      
      # 2B2. Identify whether the current unit matches the common unit
      if(curr.unit == c.unit){
        new.means<-curr.means #if the current unit matches the common unit....fill the new value and unit with the current val and unit
        new.vars<-curr.vars
        new.unit<-curr.unit
      }
      if(curr.unit != c.unit){
        #if the current unit does not match the common unit ...
        
        # 2B2a. Identify whether the current unit is in the list of convertible units
        sum(unitIndex[INDEXROWS,'conv.unit']==curr.unit)==0
        if(sum(unitIndex[INDEXROWS,'conv.unit']==curr.unit)==0){
          new.means<-rep(NA, length(curr.vars)) #if not, fill the new value with NA and the new unit with NA
          new.vars<-rep(NA, length(curr.vars))
          new.unit<-NA
          if(invnat==TRUE){
            new.means<-data.frame(measInvMean=new.means[1],measNatMean=new.means[2]) #this takes care of the problem of invaded and native values
            new.vars<-data.frame(measInvVar_VAR=new.vars[1],measNatVar_VAR=new.vars[2])
          }
        }
        if(sum(unitIndex[INDEXROWS,'conv.unit']==curr.unit)==1){
          #if it is on the list of covertible units...
          
          # 2B2a1. Index the appropriate operation needed to convert to the common unit
          THISROW<-which(unitIndex[INDEXROWS,]$conv.unit==curr.unit)
          needed.multiplier<-as.character(unitIndex[INDEXROWS[THISROW],'multiplier'])
          
          # 2B2a2. Identify whether there is division involved in the operation and create the conv.factor
          if(grepl('/', needed.multiplier,  fixed=T)){
            params<-as.numeric(unlist(strsplit(needed.multiplier, '/', fixed=T))) #if there is division...identify the numerator and denominator
            conv.factor<-params[1]/params[2] #divide the numerator by the denominator to identify the value to multiply by
          }
          if(!grepl('/', needed.multiplier,  fixed=T)){
            conv.factor<-as.numeric(needed.multiplier) #if there is not division, identify the value to multiply by
          }
          
          # 2B2a3. Multiply mean by the conv.factor to convert the mean into c.unit
          new.means<-curr.means * conv.factor
          new.vars<-curr.vars * conv.factor^2 #remember that Var(aX) = a^2 * Var(X)
          new.unit<-as.character(c.unit)
        }
      }
      
      # 2B3. Put the new values into the appropriate rows and columns of df
      if(invnat==TRUE){
        df[CATROWS[k],c('stdmeanInv','stdmeanNat')]<-new.means
        df[CATROWS[k],c('stdvarInv','stdvarNat')]<-new.vars
        df[CATROWS[k],'stdunit']<-new.unit
      }
      if(invnat==FALSE){
        df[CATROWS[k],'stdmean']<-new.means
        df[CATROWS[k],'stdvar']<-new.vars
        df[CATROWS[k],'stdunit']<-new.unit
      }
    }
  }
  
  # Return df
  return(df)
}

