#fxn_4_UnitProb.R
#Functions to figure out if the units differ between the rows that need to be aggregated




### Create function for identifying unit problems (2 identifiers) ####################################################
# sub.Agg = rows to be aggregated

UnitProb<-function(sub.Agg){
  
  # IDENTIFIER 1
  idu1<-sub.Agg[,'identifier1unit']
  numU1<-sum(!duplicated(idu1)) #number of unique ids in here
  #if >1, then that means there was a difference in units between the rows
  
  #IDENTIFIER 2
  idu2<-sub.Agg[,'identifier2unit']
  numU2<-sum(!duplicated(idu2)) #number of unique ids in here
  #if >1, then that means there was a difference in units between the rows
  
  sum(colnames(sub.Agg) %in% 'identifier3unit')>0
  if(sum(colnames(sub.Agg) %in% 'identifier3unit')>0){
    #IDENTIFIER 3
    idu3<-sub.Agg[,'identifier3unit']
    numU3<-sum(!duplicated(idu3)) #number of unique ids in here
    #if >1, then that means there was a difference in units between the rows
  }
  
  if(sum(colnames(sub.Agg) %in% 'identifier3unit')>0){
    if(sum(numU1, numU2, numU3)>3){unitProblem<-1}else{unitProblem<-0}
  }else{
    if(sum(numU1, numU2)>2){unitProblem<-1}else{unitProblem<-0}
  }
  
  return(unitProblem)
}



