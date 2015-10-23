#results/fxn_SaveFitStats.R

SaveFitStats<-function(res, res0, k, j, i){
  
  plantType<-PLANT[k]
  traitType<-TRAIT[j]
  measType<-MEASCAT[i]
  
  est<-round(res$b['xval',], digits=2)
  pVal<-round(res$pval[2], digits=2)
  studies<-res$k
  
  #McFadden's pseudo-R-squared using loglik (aka likelihood ratio index)
  #see http://www.ats.ucla.edu/stat/mult_pkg/faq/general/Psuedo_RSquareds.htm
  ll<-res$fit.stats['ll','ML']
  ll0<-res0$fit.stats['ll','ML']
  pR2<-round(1-(ll/ll0), digits=2)
  
  result<-data.frame(plantType, traitType, measType, studies, pR2, est, pVal)
  return(result)
}