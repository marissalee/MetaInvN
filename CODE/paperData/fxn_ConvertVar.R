#ConvertVar.R
#Functions to convert measures of variation (SE, SD, etc) to Var




### Create functions convert SE and SD values to variance ####################################################
SEtoVar<-function(se.val, n.val){
  var.val <- (se.val * sqrt(n.val))^2
  return(var.val)
}
SDtoVar<-function(sd.val){
  var.val <- sd.val^2
  return(var.val)
}
CItoVar<-function(ci.val, n.val){
  se.val <- ci.val / 1.96 
  var.val <- (se.val * sqrt(n.val))^2
  return(var.val)
}
CVtoVar<-function(cv.val, mean.val){ # CV = SD/mean
  sd.val<-cv.val * mean.val
  var.val <- sd.val^2
  return(var.val)
}
# ThrdQuartiletoVar<-function(quar.val){ # No way to estimate variance from 3rd quartile value
#   var.val<-rep(NA,length(quar.val))
#   return(var.val)
# }



### Create a function to convert SE, SD, etc to variance by aggID ####################################################
#spit out a vector of variance that can be added onto the end of the dataframe
#spit out a warning if there is more than 1 variance type an aggID
# vartype.colnam <-'traitVarType'
# varval.colnam <- 'ta_Var'
# n.colnam <- 'ta_N'
# mean.colnam <- 'ta_Mean'
FixVarTypes<-function(df, dfAgg, vartype.colnam, varval.colnam, n.colnam, mean.colnam){
  
  AGGID<-unique(dfAgg$aggID) # to loop through each aggID
  curr.vars<-numeric(0)
  i<-0
  for (i in 1:length(AGGID)){
    
    #identify the vartype
    var.type<-df[df$aggID == AGGID[i], vartype.colnam]
    
    #pull out the current var values and n values
    var.val<-dfAgg[dfAgg$aggID == AGGID[i], varval.colnam]
    n.val<-dfAgg[dfAgg$aggID == AGGID[i], n.colnam]
    mean.val<-dfAgg[dfAgg$aggID == AGGID[i], mean.colnam]
    
    #convert to VAR
    if(!is.na(var.type) & var.type=='SE'){curr.var.vec<-SEtoVar(var.val,n.val)}
    if(!is.na(var.type) & var.type=='SD'){curr.var.vec<-SDtoVar(var.val)}
    if(!is.na(var.type) & var.type=='95CI'){curr.var.vec<-CItoVar(var.val,n.val)}
    if(!is.na(var.type) & var.type=='Var'){curr.var.vec<-var.val}
    if(!is.na(var.type) & var.type=='CV'){curr.var.vec<-CVtoVar(var.val,mean.val)}
    if(is.na(var.type)){
      var.val[is.na(var.val)|!is.na(var.val)]<-NA
      curr.var.vec<-var.val
    }
    
    #save the curr.var.vec
    if(length(varval.colnam) == 1){ #for trait data 
      curr.vars<-c(curr.vars, curr.var.vec) 
    }
    if(length(varval.colnam) == 2){ #for cover and measure data
      curr.vars<-rbind(curr.vars,curr.var.vec)
    }
  }
  return(curr.vars)
}




### Create a function to convert SE, SD, etc to variance by row ####################################################
#iteratively fill out the variance column the end of the dataframe
# df<-measures
# vartype.colnam<-'measVarType'
# varval.colnam<-c('measInvVar','measNatVar')
# n.colnam<-(c('measInvN','measNatN'))
FixVarTypes.Row<-function(df,vartype.colnam, varval.colnam, n.colnam, mean.colnam){
  
  # Clean dataset so that if there is data in vartype, but no values, then replace vartype with NA temporarily
  if(length(varval.colnam) == 1){ #for trait data 
    df[!is.na(df[,vartype.colnam]) & is.na(df[,varval.colnam[1]]),vartype.colnam]<-NA
  }
  if(length(varval.colnam) == 2){ #for cover and measure data
    df[!is.na(df[,vartype.colnam]) & is.na(df[,varval.colnam[1]]) & is.na(df[,varval.colnam[2]]),vartype.colnam]<-NA
  }
  
  # Clean dataset so that if there is NO data in vartype, but there ARE values, then replace varvals with NAs temporarily
  if(length(varval.colnam) == 1){ #for trait data 
    df[is.na(df[,vartype.colnam]) & !is.na(df[,varval.colnam[1]]),vartype.colnam]<-NA
  }
  if(length(varval.colnam) == 2){ #for cover and measure data
    df[is.na(df[,vartype.colnam]) & !is.na(df[,varval.colnam[1]]),varval.colnam[1]]<-NA
    df[is.na(df[,vartype.colnam]) & !is.na(df[,varval.colnam[2]]),varval.colnam[2]]<-NA
  }
  
  # Set up parameters
  ROW<-dim(df)[1] # to loop through each row in df
  curr.vars<-numeric(0)
  i<-0
  for (i in 1:ROW){
    
    # Pull out the vartype, varvals, and nvals for that row
    var.type<-df[i, vartype.colnam]
    var.val<-df[i, varval.colnam]
    n.val<-df[i, n.colnam]
    mean.val<-df[i, mean.colnam]
    
    # Convert variance measures to VAR
    if(!is.na(var.type) & var.type=='SE'){curr.var.vec<-SEtoVar(var.val,n.val)}
    if(!is.na(var.type) & var.type=='SD'){curr.var.vec<-SDtoVar(var.val)}
    if(!is.na(var.type) & var.type=='95CI'){curr.var.vec<-CItoVar(var.val,n.val)}
    if(!is.na(var.type) & var.type=='Var'){curr.var.vec<-var.val}
    if(!is.na(var.type) & var.type=='CV'){curr.var.vec<-CVtoVar(var.val,mean.val)}
    
    if(is.na(var.type) | var.type=='notReported'){
      var.val[is.na(var.val)|!is.na(var.val)]<-NA
      curr.var.vec<-var.val
    }
    
    # Save the curr.var.vec
    if(length(varval.colnam) == 1){ #for trait data 
      curr.vars<-c(curr.vars, curr.var.vec) 
    }
    if(length(varval.colnam) == 2){ #for cover and measure data
      curr.vars<-rbind(curr.vars,curr.var.vec)
    }
  }
  
  # Return the columns of curr.vars (same length as the dim(df)[1])
  return(curr.vars)
}




### Create function to convert variance to SD ####################################################
VARtoSD <- function(var){
  sd <- sqrt(var)
  return(sd)
}









