#results/fxn_LoopModels.R
#Functions to run models

FitModel<-function(dat1, k, TRAIT, MEASCAT, factor){
  
  list.res<-list()
  list.resREML<-list()
  list.resfac<-list()
  list.anova<-list()
  list.res1<-list()
  list.resREML1<-list()
  list.resfac1<-list()
  list.anova1<-list()
  
  j<-0 #TRAITS
  for(j in 1:length(TRAIT)){
    i<-0 #MEASCAT
    for(i in 1:length(MEASCAT)){
      
      #subset data by the current effect size measurement and trait value
      dat2<-subset(dat1, measCat==MEASCAT[i] & traitCat==TRAIT[j])
      
      #simplify dataframe
      paperID<-dat2$paperID
      obsID<-dat2$obsID
      fac<-dat2[,factor]
      xval<-dat2[,PLANT[k]]
      yi<-dat2$yi
      vi<-dat2$vi
      data<-data.frame(paperID, obsID, fac, xval, yi, vi)
      data1<-data[!is.na(data$yi) & !is.na(data$vi) & !is.na(data$xval),] #remove NAs for base dataset
      data1b<-data[!is.na(data$yi) & !is.na(data$vi) & !is.na(data$xval) & !is.na(data$fac),] #remove NAs for base dataset
      
      if(dim(data1)[1]>2 & length(unique(data1$xval))!=1){
        
        #fit a meta-regression with TRAIT as x axis
        res <-rma.mv(yi, vi, 
                     mods = ~ xval, 
                     random=list(~1 | paperID, ~1 | obsID), 
                     data=data1b, slab=as.character(obsID), method="ML")
        list.res[[i]] <- res
        
        resREML <-rma.mv(yi, vi, 
                     mods = ~ xval, 
                     random=list(~1 | paperID, ~1 | obsID), 
                     data=data1, slab=as.character(obsID))
        list.resREML[[i]] <- resREML
        
        resfac<- rma.mv(yi, vi, 
                        mods = ~ xval * fac, 
                        random=list(~1 | paperID, ~1 | obsID), 
                        data=data1b, slab=as.character(obsID), method="ML")
        list.resfac[[i]]<-resfac
        
        if(res$parms != resfac$parms){
          list.anova[[i]]<-anova(res, resfac)
        }else{list.anova[[i]]<-NA}
      }
    }
    names(list.res)<-MEASCAT
    names(list.resREML)<-MEASCAT
    names(list.resfac)<-MEASCAT
    names(list.anova)<-MEASCAT
    
    list.res1[[j]]<-list.res
    list.resREML1[[j]]<-list.resREML
    list.resfac1[[j]]<-list.resfac
    list.anova1[[j]]<-list.anova
  }
  names(list.res1)<-TRAIT
  names(list.resREML1)<-TRAIT
  names(list.resfac1)<-TRAIT
  names(list.anova1)<-TRAIT
  result<-list(res=list.res1, resREML=list.resREML1, resfac=list.resfac1, anova=list.anova1)
  
  return(result)
  
}

SignifQM<-function(result, restype, facName){
  
  stored<-numeric(0)
  
  j<-0
  for(j in 1:length(TRAIT)){
    
    i<-0
    for(i in 1:length(MEASCAT)){
      tmp<-result[[restype]][[j]][[i]]
      
      if(restype %in% c('res','resfac','resPlot','resREML')){
        if(tmp$QMp<0.1){
          store<-data.frame(fac=facName, trait=TRAIT[j], meas=MEASCAT[i], pval=round(tmp$QMp,3))
          stored<-rbind(stored,store)
        }
      }
      
      if(!is.na(tmp)){
        if(tmp$pval<0.1){
          store<-data.frame(fac=facName, trait=TRAIT[j], meas=MEASCAT[i], pval=round(tmp$pval,3))
          stored<-rbind(stored,store)
        }
      }
      
    }
  }
  
  return(stored)
}

