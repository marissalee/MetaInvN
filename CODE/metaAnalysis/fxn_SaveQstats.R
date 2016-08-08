#Save Q stats FXN: Extract model heterogeneity info out of the list of model results
SaveQstats<-function(res.list){
  
  #extract parameters from res.list and make into a dataframe
  result.list<-list()
  i<-0
  for(i in 1:length(res.list)){
    
    #pull out a model
    res<-res.list[[i]]
    
    #identify stats
    QE<-round(res$QE, 3)
    QEdf<-res$k -1
    QEp<-round(res$QEp, 3)
    QM<-round(res$QM,3)
    QMp<-round(res$QMp,3)

    #save everything in a list
    result.list[[i]]<-c(QE, QEdf, QEp, QM, QMp)
  }
  resultdf<-ldply(result.list, rbind.fill)
  colnames(resultdf)<-c("QE", "QEdf", "QEp", "QM", "QMp")
  
  #add MEASCAT labels
  resultdf<-data.frame(measCAT=MEASCAT, labels, resultdf)
  resultdf$measCAT<-factor(resultdf$measCAT, levels=MEASCAT)
  
  return(resultdf)
  
}

