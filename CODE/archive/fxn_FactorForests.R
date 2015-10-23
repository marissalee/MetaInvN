## Fxn to look at models a modifying factor is warrented

FactorForest<-function(MeasFac, res.fac, dat.tmp, facCol){
  
  #extract and save model stats
  fac.list<-list() #save the beta and pvals for each term in full model
  data.list<-list() #test of each level relative to 0
  
  #loop through each measurement for which the factor is significant (e.g. no, som, biom)
  i<-0
  for(i in 1:length(MeasFac)){
    
    ###For a given measurement, pull out the model coefficients and pvalues
    res<-res.fac[[as.character(MeasFac[i])]]
    tab<-round(cbind(res$b,res$pval), 3)
    colnames(tab)<-c('beta','pval')
    fac.list[[i]]<-tab
    
    ###For a given measurement, subset each level of the factor (e.g. field study) and test whether its effect size differs from 0
    dat.tmp1<-dat.tmp #rename
    colnames(dat.tmp1)[colnames(dat.tmp1)==facCol]<-'facCol' #rename factor column
    subdat<-subset(dat.tmp1, measCat==MeasFac[i]) #subset by meas
    subdat1<-subdat[!is.na(subdat$yi) & !is.na(subdat$vi) & !is.na(subdat$facCol),] #remove rows with NA
    CAT<-unique(subdat1$facCol)
    #identify singularities, remove factors with only 1 study
    summ<-ddply(subdat1, ~facCol, summarize, n=length(facCol))
    if(sum(summ$n==1)>0){
      exclude<-summ[summ$n == 1,'facCol']
      CAT<-CAT[CAT != exclude]
    }
    est.list<-list()
    var.list<-list()
    cil.list<-list()
    ciu.list<-list()
    k.list<-list()
    #loop through each level of the factor
    j<-0 
    for(j in 1:length(CAT)){
      subdat.sub<-subset(subdat1, facCol==CAT[j])
      res1 <- rma.mv(yi, vi, random=list(~1 | paperID, ~1 | obsID), 
                     data=subdat.sub, control=list(optimizer="nlminb", 
                                                   iter.max=1000,
                                                   sigma2.init=c(1,1)))
      est.list[[j]]<-coef(res1)
      var.list[[j]]<-vcov(res1)
      cil.list[[j]]<-res1$ci.lb
      ciu.list[[j]]<-res1$ci.ub
      k.list[[j]]<-res1$k  
    }
    #unlist all of the results
    estimates<-unlist(est.list)
    variances<-unlist(var.list)
    cil<-unlist(cil.list)
    ciu<-unlist(ciu.list)
    ks<-unlist(k.list)
    #make a new dataframe with the results to test whether its effect size differs from 0
    ggdf<-data.frame(CAT, estimates, variances, cil, ciu, ks)
    data.list[[i]]<-ggdf
    
  }
  names(fac.list)<-MeasFac
  names(data.list)<-MeasFac
  
  #return the model stats
  return(data.list)
}