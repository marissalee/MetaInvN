#paperData_aggregate/fxn_2_Agg.R
#Functions to aggregate means and variances

#This depends on the functions in paperData_aggregate/fxn_1_Calcs.R
#CalcMean
#CalcSum
#CalcDivide
#CalcPercent
#CalcConvertNH
#CalcConvertNO


### Create function that calculate the global mean and variance given an operation ####################################################
# dfagg<-group.op.rows
# oper<-operationType1
# op.params<-op.param.list[[1]]
#invnat = TRUE or FALSE.... true for data with invmean and natmean, false for data with mean only
Aggregate<-function(oper, dfagg, op.params, invnat){
  
  # IDENTIFY PARAMETERS
  colmean<-op.params$colmean
  colvar<-op.params$colvar
  coln<-op.params$coln
  colorder<-op.params$colorder
  colid<-op.params$colid
  
  
  # If more than 1 row ...  
  # ASSIGN CURRENT VALUES, DO OPERATION, ASSIGN global VALUES
  if(dim(dfagg)[1]>1){ 
    
    # Current values for average and addition operations
    if(oper == 'average' | oper == 'add'){
      group.means<-dfagg[,colmean]
      group.vars<-dfagg[,colvar]
      group.ns<-dfagg[,coln]
    }
    
    # Current values for division, calcPercent, etc operations
    if(oper == 'divide' | oper == 'calcPercent' | oper == 'convertTo_nh' | oper == 'convertTo_no' | oper == 'subtract'){
      
      #figure out the order the row need to operated on
      if(length(unique(dfagg[,colorder]))>1){print('warning')} #if there is more than 1 way to order the identifiers for this operation, spit out a warning
      temporder<-strsplit(as.character(dfagg[,colorder]), ',')[1] #first object in the list, all objects should be the same
      r1<-dfagg[dfagg[,colid] == temporder[[1]][1],]
      r2<-dfagg[dfagg[,colid] == temporder[[1]][2],]
      
      #identify current values
      group.means1<-r1[,colmean]
      group.means2<-r2[,colmean]
      group.vars1<-r1[,colvar]
      group.vars2<-r2[,colvar]
      group.ns1<-r1[,coln]
      group.ns2<-r2[,coln]
    }
    
    # Do the operation on the current values, organize based on invnat type, assign global values
    if(invnat==FALSE){
      
      # Operate
      if(oper == 'average'){global.list<-CalcMean(group.means, group.vars, group.ns)}
      if(oper == 'add'){global.list<-CalcSum(group.means, group.vars, group.ns)}
      if(oper == 'divide'){global.list<-CalcDivide(group.means1,group.means2, group.vars1, group.vars2, group.ns1, group.ns2)}
      if(oper == 'calcPercent'){global.list<-CalcPercent(group.means1,group.means2, group.vars1, group.vars2, group.ns1, group.ns2)}
      if(oper == 'convertTo_nh'){global.list<-CalcConvertNH(group.means1,group.means2, group.vars1, group.vars2, group.ns1, group.ns2)}
      if(oper == 'convertTo_no'){global.list<-CalcConvertNO(group.means1,group.means2, group.vars1, group.vars2, group.ns1, group.ns2)}
      if(oper == 'subtract'){global.list<-CalcSubtract(group.means1,group.means2, group.vars1, group.vars2, group.ns1, group.ns2)}
      
      if(!oper %in% c('average','add','divide','calcPercent', 'convertTo_nh','convertTo_no', 'subtract')){global.list<-CalcSum(group.means, group.vars, group.ns)} # DEFAULTS TO CALCSUM, IF IT IS AN UNUSUAL OPERATION
      
      # Assign global values
      global.mean<-global.list$global.mean
      global.var<-global.list$global.var
      global.n<-global.list$global.n
      
    }
    if(invnat==TRUE){
      
      # Operate
      if(oper == 'average'){
        global.Invs<-CalcMean(group.means[,1], group.vars[,1], group.ns[,1])
        global.Nats<-CalcMean(group.means[,2], group.vars[,2], group.ns[,2])
      }
      if(oper == 'add'){
        global.Invs<-CalcSum(group.means[,1], group.vars[,1], group.ns[,1])
        global.Nats<-CalcSum(group.means[,2], group.vars[,2], group.ns[,2])
      }
      if(oper == 'divide'){
        global.Invs<-CalcDivide(group.means1[,1], group.means2[,1], group.vars1[,1], group.vars2[,1], group.ns1[,1], group.ns2[,1])
        global.Nats<-CalcDivide(group.means1[,2], group.means2[,2], group.vars1[,2], group.vars2[,2], group.ns1[,2], group.ns2[,2])
      }
      if(oper == 'calcPercent'){
        global.Invs<-CalcPercent(group.means1[,1], group.means2[,1], group.vars1[,1], group.vars2[,1], group.ns1[,1], group.ns2[,1])
        global.Nats<-CalcPercent(group.means1[,2], group.means2[,2], group.vars1[,2], group.vars2[,2], group.ns1[,2], group.ns2[,2])
      }
      if(oper == 'convertTo_nh'){
        global.Invs<-CalcConvertNH(group.means1[,1], group.means2[,1], group.vars1[,1], group.vars2[,1], group.ns1[,1], group.ns2[,1])
        global.Nats<-CalcConvertNH(group.means1[,2], group.means2[,2], group.vars1[,2], group.vars2[,2], group.ns1[,2], group.ns2[,2])
      }
      if(oper == 'convertTo_no'){
        global.Invs<-CalcConvertNO(group.means1[,1], group.means2[,1], group.vars1[,1], group.vars2[,1], group.ns1[,1], group.ns2[,1])
        global.Nats<-CalcConvertNO(group.means1[,2], group.means2[,2], group.vars1[,2], group.vars2[,2], group.ns1[,2], group.ns2[,2])
      }
      if(oper == 'subtract'){
        global.Invs<-CalcSubtract(group.means1[,1], group.means2[,1], group.vars1[,1], group.vars2[,1], group.ns1[,1], group.ns2[,1])
        global.Nats<-CalcSubtract(group.means1[,2], group.means2[,2], group.vars1[,2], group.vars2[,2], group.ns1[,2], group.ns2[,2])
      }
      if(!oper %in% c('average','add','divide','calcPercent','convertTo_nh','convertTo_no','subtract')){ # DEFAULTS TO CALCSUM, IF IT IS AN UNUSUAL OPERATION
        global.Invs<-CalcSum(group.means[,1], group.vars[,1], group.ns[,1])
        global.Nats<-CalcSum(group.means[,2], group.vars[,2], group.ns[,2])
      }
      
      # Assign global values
      global.Inv.mean<-global.Invs$global.mean
      global.Inv.var<-global.Invs$global.var
      global.Inv.n<-global.Invs$global.n
      global.Nat.mean<-global.Nats$global.mean
      global.Nat.var<-global.Nats$global.var
      global.Nat.n<-global.Nats$global.n
    } 
    
  }
  
  
  # If only 1 row ...
  # ASSIGN global VALUES
  if(dim(dfagg)[1]==1){
    
    # Assign global values with the old values, organize based on invnat type
    if(invnat==FALSE){
      global.mean<-dfagg[,colmean]
      global.var<-dfagg[,colvar]
      global.n<-dfagg[,coln]
    }
    if(invnat==TRUE){
      global.Inv.mean<-dfagg[,colmean[1]]
      global.Inv.var<-dfagg[,colvar[1]]
      global.Inv.n<-dfagg[,coln[1]]
      global.Nat.mean<-dfagg[,colmean[2]]
      global.Nat.var<-dfagg[,colvar[2]]
      global.Nat.n<-dfagg[,coln[2]]
    }
    
  }
  
  
  # STORE EVERYTHING IN A DATAFRAME CALLED RESULTS
  if(invnat==FALSE){
    results<-data.frame(aggID=unique(dfagg[,'aggID']), oper=oper, mean=global.mean, var=global.var, n=global.n)
  }
  if(invnat==TRUE){
    results<-data.frame(aggID=unique(dfagg[,'aggID']), oper=oper, 
                        Inv.mean=global.Inv.mean, Inv.var=global.Inv.var, Inv.n=global.Inv.n,
                        Nat.mean=global.Nat.mean, Nat.var=global.Nat.var, Nat.n=global.Nat.n)
  }
  
  
  # RETURN RESULTS
  return(results)
  
}
