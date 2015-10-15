#paperData_aggregate/fxn_3_NumOps.R
#Functions to aggregate means and variances with 1 operation, 2 operations, 3 operations




### Create function that for numobs == 1 ####################################################
# sub.Agg = rows to be aggregated
# colop1<-'operation1'
# colorder1<-'operation1order'
# colid1<-'identifier1'
# invnat = TRUE/FALSE
NumOp1<-function(sub.Agg, colop1, colorder1, colid1, invnat){
  
  # DO OPERATION 1 OF 1
  
  # Define parameters
  oper<-unique(sub.Agg[,colop1]) #operation type
  dfagg<-sub.Agg #rows to be aggregated
  op.params<-list(colmean=colmean1, #column name index.. should be 1s since operation 1
                  colvar=colvar1,
                  coln=coln1,
                  colorder=colorder1,
                  colid=colid1)
  
  # Aggregate
  result<-Aggregate(oper=oper,
                    dfagg=dfagg,
                    op.params=op.params,
                    invnat=invnat)
  
  # RETURN RESULT
  return(result)
}




### Create function that for numobs == 2 ####################################################
# sub.Agg = rows to be aggregated
# colop1<-'operation1'
# colorder1<-'operation1order'
# colid1<-'identifier1'
# colop2<-'operation2'
# colorder2<- 'operation2order'
# colid2<-'identifier2'
# invnat = TRUE/FALSE
NumOp2<-function(sub.Agg, colop1, colorder1, colid1, colop2, colorder2, colid2, invnat){
  
  # DO OPERATION 1 OF 2
  
  # Define parameters
  oper<-unique(sub.Agg[,colop1])[!is.na(unique(sub.Agg[,colop1]))] #operation type
  ROWGROUP<-unique(sub.Agg[,colid2]) #identity of rows to be aggregated; notice that this is identifier2, not 1
  op.params<-list(colmean=colmean1, #column name index.. should be 1s since operation 1
                  colvar=colvar1,
                  coln=coln1,
                  colorder=colorder1,
                  colid=colid1)
  
  # Aggregate: Loop through each ROWGROUP
  ROWGROUP.result.list<-list()
  k<-0
  for (k in 1:length(ROWGROUP)){
    curr.op.rows<-sub.Agg[sub.Agg[,colid2]==ROWGROUP[k],] #subset the group of rows to be aggregated
    result<-Aggregate(oper=oper,
                      dfagg=curr.op.rows,
                      op.params=op.params,
                      invnat=invnat)
    ROWGROUP.result.list[[as.character(ROWGROUP[k])]]<-result
  }
  op1results<-ldply(ROWGROUP.result.list, data.frame) #change it from a list into a data.frame
  
  
  # DO OPERATION 2 OF 2
  
  # Define parameters
  oper<-unique(sub.Agg[,colop2])[!is.na(unique(sub.Agg[,colop2]))] #operation type
  op1results$oper2<-rep(oper, dim(op1results)[1]) #1) add the operation type to op1results
  op1results$op2order<-rep(unique(sub.Agg[,colorder2]), dim(op1results)[1]) #2) add the operation order to op1results
  op.params<-list(colmean=colmean2, #column name index.. should be 2s since operation 2
                  colvar=colvar2,
                  coln=coln2,
                  colorder='op2order', #update the colorder
                  colid='.id') #update the colid
  
  # Aggregate
  results<-Aggregate(oper=oper,
                     dfagg=op1results,
                     op.params=op.params,
                     invnat=invnat)
  
  
  # RETURN RESULT
  return(results)
}




### Create function that for numobs == 3 ####################################################
# sub.Agg = rows to be aggregated
# colop1<-'operation1'
# colorder1<-'operation1order'
# colid1<-'identifier1'
# colop2<-'operation2'
# colorder2<- 'operation2order'
# colid2<-'identifier2'
# colop3<-'operation3'
# colorder3<- 'operation3order'
# colid3<-'identifier3'
# invnat = TRUE/FALSE
NumOp3<-function(sub.Agg, colop1, colorder1, colid1, colop2, colorder2, colid2, colop3, colorder3, colid3, invnat){
  
  # DO OPERATION 1 of 3
  
  # Define parameters
  oper<-unique(sub.Agg[,colop1])[!is.na(unique(sub.Agg[,colop1]))] #operation type
  sub.Agg$rowgrpcol1<-paste(sub.Agg[,colid2],sub.Agg[,colid3], sep="_") #paste together identifier 2 and 3
  ROWGROUP<-unique(sub.Agg$rowgrpcol1) #identity of rows to be aggregated
  op.params<-list(colmean=colmean1, #column name index ... should be 1s since operation 1
                  colvar=colvar1,
                  coln=coln1,
                  colorder=colorder1, #update the colorder
                  colid=colid1) #update the colorder
  
  # Aggregate: Loop through each ROWGROUP
  ROWGROUP.result.list<-list()
  k<-0
  for (k in 1:length(ROWGROUP)){
    curr.op.rows<-sub.Agg[sub.Agg$rowgrpcol1==ROWGROUP[k],] #subset the group of rows to be aggregated
    ROWGROUP.result.list[[as.character(ROWGROUP[k])]]<-Aggregate(oper=oper,
                                                                 dfagg=curr.op.rows,
                                                                 op.params=op.params,
                                                                 invnat=invnat)
  }
  op1results<-ldply(ROWGROUP.result.list, data.frame) #change it from a list into a data.frame
  
  # DO OPERATION 2 OF 3
  
  # Define parameters
  oper<-unique(sub.Agg[,colop2])[!is.na(unique(sub.Agg[,colop2]))] #operation type
  op1results$oper2<-rep(oper, dim(op1results)[1]) #add the operation type to op1results
  op1results$op2order<-rep(unique(sub.Agg[,colorder2]), dim(op1results)[1]) #add the operation order to op1results
  id23tab<-ldply(strsplit(op1results$.id,'_'), rbind.fill)
  colnames(id23tab)<-c('id2','id3')
  op1results<-data.frame(op1results, .id2=id23tab[,'id3']) #add the identifiers for operation2 to the op1results table
  ROWGROUP<-unique(op1results$.id2)
  op.params<-list(colmean=colmean2, #column name index ... should be 2s since operation 2
                  colvar=colvar2,
                  coln=coln2,
                  colorder='op2order', #update the colorder
                  colid='.id2') #update the colorder
  # Aggregate: Loop through each ROWGROUP
  ROWGROUP.result.list<-list()
  k<-0
  for (k in 1:length(ROWGROUP)){
    curr.op.rows<-op1results[op1results$.id2==ROWGROUP[k],] #subset the group of rows to be aggregated
    result<-Aggregate(oper=oper,
                      dfagg=curr.op.rows,
                      op.params=op.params,
                      invnat=invnat)
    ROWGROUP.result.list[[as.character(ROWGROUP[k])]]<-result
  }
  op2results<-ldply(ROWGROUP.result.list, data.frame) #change it from a list into a data.frame
  
  #DO OPERATION 3 OF 3
  
  # Define parameters
  oper<-unique(sub.Agg[,colop3])[!is.na(unique(sub.Agg[,colop3]))] #operation type
  op2results$oper3<-rep(oper, dim(op2results)[1]) #add the operation type to op2results
  op2results$op3order<-rep(unique(sub.Agg[,colorder3]), dim(op2results)[1]) #add the operation order to op2results
  op2results<-data.frame(op2results, .id3=op2results$.id) #add the identifiers for operation2 to the op1results table
  op.params<-list(colmean=colmean2, #column name index.. should be 2s since operation 3
                  colvar=colvar2,
                  coln=coln2,
                  colorder='op3order', #update the colorder
                  colid='.id3') #update the colid
  
  # Aggregate
  results<-Aggregate(oper=oper,
                     dfagg=op2results,
                     op.params=op.params,
                     invnat=invnat)
  
  # RETURN RESULT
  return(results)
}
