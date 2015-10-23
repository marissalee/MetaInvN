## Fxns to make a global forest plot and save estimates

#PrepForestPlot FXN: Extract info out of the list of model results and create a dataframe to use for plotting
PrepForestPlot<-function(res.list){
  
  #extract parameters from res.list and make into a dataframe
  result.list<-list()
  i<-0
  for(i in 1:length(res.list)){
    
    #pull out a model
    res<-res.list[[i]]
    
    #identify stats
    est<-round(res$b, 3)
    var<-round(vcov(res),3)
    cil<-round(res$ci.lb, 3)
    ciu<-round(res$ci.ub,3)
    k<-res$k  
    p.val<-round(res$pval, 3)
    intraPaperCorr<-round(res$sigma2[1] / sum(res$sigma2), 3) #if this # is high, it means that the underlying true effects within paperIDs are estimated to correlate quite strongly
    Heterogen<-round(sum(res$sigma2), 3) #the sum of the two variance components can be interpreted as the total amount of heterogeneity in the true effects
    
    #save everything in a list
    result.list[[i]]<-c(est, var, cil, ciu, k, p.val, intraPaperCorr, Heterogen)
  }
  resultdf<-ldply(result.list, rbind.fill)
  colnames(resultdf)<-c("est","var","cil","ciu","k","pval","intraPaperCorr","heterogen")
  
  #add MEASCAT labels
  MEASCAT
  resultdf<-data.frame(measCAT=MEASCAT, labels, resultdf)
  resultdf$measCAT<-factor(resultdf$measCAT, levels=MEASCAT)
  
  #column that indicates whether the CI overlaps 0
  overlap0<-resultdf$cil<0 &  resultdf$ciu>0
  resultdf$NoOverlap0<-!overlap0
  
  #column that indicates whether the pval<0.1, pval<0.05
  resultdf$alpha05<-resultdf$pval<0.05
  
  return(resultdf)
  
}




