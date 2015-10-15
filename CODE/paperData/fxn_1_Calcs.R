#paperData_aggregate/fxn_1_Calcs.R
#Functions to aggregate Mean, Var, N




### Calculate the global mean by averaging group means ####################################################
CalcMean<-function(group.means, group.vars, group.ns){
  
  #Global N
  global.n <- min(group.ns, na.rm=T)
  
  #Global Mean
  global.mean <- sum(group.ns * group.means, na.rm=T) / sum(group.ns, na.rm=T)
  
  #Global Var
  ESS <- sum(group.vars * (group.ns - 1), na.rm=T)
  TGSS <- sum((group.means - global.mean)^2 * group.ns, na.rm=T)
  global.var <- ESS + TGSS / (global.n - 1)

  results<-data.frame(global.mean, global.var, global.n)
  return(results)
}




### Calculate the global mean by summing group means ####################################################
CalcSum<-function(group.means, group.vars, group.ns){
  
  #Global N
  global.n<-min(group.ns, na.rm=T)
  
  #Global Mean
  global.mean<-sum(group.means, na.rm=T)
  if(sum(is.na(group.means))==2){global.mean<-NA}
  
  #Global Var
  global.var <- sum(group.vars, na.rm=T)
  
  results<-data.frame(global.mean, global.var, global.n)
  return(results)
}




### Calculate the global mean by dividing group means ####################################################
CalcDivide<-function(group.means1, group.means2, group.vars1, group.vars2, group.ns1, group.ns2){
  
  #Global N
  global.n<-min(c(group.ns1, group.ns2), na.rm=T)
  
  #Global Mean
  global.mean<-group.means1/group.means2
  
  #Global Var
  global.var <- prod(c(group.vars1, group.vars2) + c(group.means1,(1/group.means2))^2) - prod(c(group.means1, (1/group.means2))^2) # if you put na.rm=T and you have a situation where 1 row has var=NA and mean=X, then you will end up with negative aggregated var
  
  results<-data.frame(global.mean, global.var, global.n)
  return(results)
}




### Calculate percentage ####################################################
CalcPercent<-function(group.means1, group.means2, group.vars1, group.vars2, group.ns1, group.ns2){
  
  #Global N
  global.n<-min(c(group.ns1, group.ns2), na.rm=T)
  
  #Global Mean
  global.mean<-(group.means1/group.means2)*100
  
  #Global Var
  global.var.step1 <- prod(c(group.vars1, group.vars2) + c(group.means1,(1/group.means2))^2) - prod(c(group.means1, (1/group.means2))^2) # if you put na.rm=T and you have a situation where 1 row has var=NA and mean=X, then you will end up with negative aggregated var
  global.var <- global.var.step1 * 100^2 #remember that Var(aX) = a^2 * Var(X)
  
  results<-data.frame(global.mean, global.var, global.n)
  return(results)
}




### Calculate nh from toti value and nh% ####################################################
## Multiply toti (mmol/kg) by the %nh to get nh (mmol/kg)
CalcConvertNH<-function(group.means1, group.means2, group.vars1, group.vars2, group.ns1, group.ns2){
  
  #Global N
  global.n<-min(c(group.ns1, group.ns2), na.rm=T)
  
  #Global Mean
  global.mean<-group.means1 * group.means2
  
  #Global Var
  ESS <- sum(group.vars1 * (group.ns1 - 1), na.rm=T)
  TGSS <- sum((group.means1 - global.mean)^2 * group.ns1, na.rm=T)
  global.var <- ESS + TGSS / (global.n - 1)
  
  results<-data.frame(global.mean, global.var, global.n)
  return(results)
}




### Calculate no from toti value and nh% ####################################################
## Multiply toti (mmol/kg) by the %nh to get nh (mmol/kg), subtract nh (mmol/kg) from toti (mmol/kg) to get no(mmol/kg)
CalcConvertNO<-function(group.means1, group.means2, group.vars1, group.vars2, group.ns1, group.ns2){
  
  #where group.means1 == toti (mmol/kg) and group.means2 = %nh
  
  #Global N
  global.n<-min(c(group.ns1, group.ns2), na.rm=T)
  
  #Global Mean
  global.mean.nh<-group.means1 * group.means2
  global.mean<-group.means1 -  global.mean.nh 
  
  #Global Var
  ESS <- sum(group.vars1 * (group.ns1 - 1), na.rm=T)
  TGSS <- sum((group.means1 - global.mean)^2 * group.ns1, na.rm=T)
  global.var <- ESS + TGSS / (global.n - 1)
  
  results<-data.frame(global.mean, global.var, global.n)
  return(results)
}



### Calculate no by subtracting toti - nh ####################################################
CalcSubtract<-function(group.means1, group.means2, group.vars1, group.vars2, group.ns1, group.ns2){

  global.mean<-group.means1-group.means2 #Global Mean
  global.var <- sum(c(group.vars1, group.vars2), na.rm=T)#Global Var
  
  #Global N
  global.n<-min(c(group.ns1, group.ns2), na.rm=T)
  
  #Global Mean
  global.mean<-group.means1-group.means2
  
  #Global Var
  global.var <- sum(c(group.vars1, group.vars2), na.rm=T)
  
  results<-data.frame(global.mean, global.var, global.n)
  return(results)
}


