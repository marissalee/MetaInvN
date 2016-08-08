#results/fxn_FitPlot.R
#Functions to plot SMD (standard mean difference, y-axes) results against trait values (x-axis)

### Create function to save model fit stats in FitPlot fxn ####################################################
SaveFitStats<-function(res, res0, k, j, i){
  
  plantType<-PLANT[k]
  traitType<-TRAIT[j]
  measType<-MEASCAT[i]
  
  est<-round(res$b['xval',], digits=2)
  pVal<-res$pval[2]
  studies<-res$k
  
  #manually calculate the pseudo R2 values
  #compute the proportional reduction in the variance components as a sort of pseudo R-squared value
  sigma2.1.full<-res$sigma2[1]
  sigma2.2.full<-res$sigma2[2]
  sigma2.1.red<-res0$sigma2[1]
  sigma2.2.red<-res0$sigma2[2]
  pseudoR2.1<-(res0$sigma2[1] - res$sigma2[1]) / res0$sigma2[1]
  pseudoR2.2<-(res0$sigma2[2] - res$sigma2[2]) / res0$sigma2[2]
  if(pseudoR2.1<0){pseudoR2.1<-0}
  if(pseudoR2.2<0 | is.na(pseudoR2.2)){pseudoR2.2<-0}
  pseudoR2.perc<-sum(res0$sigma2) - sum(res$sigma2) / sum(res0$sigma2)
  if(pseudoR2.perc<0 | is.na(pseudoR2.perc)){pseudoR2.perc<-0}
  
  result<-data.frame(plantType, traitType, measType, studies, est, pVal, pseudoR2.1, pseudoR2.2, pseudoR2.perc)
  return(result)
}



### Create function to (1) subset data for each model, (2) run the meta-regression, (3) plot it ####################################################
#depends on SaveFitStats fxn
FitPlot<-function(dat1, k, qualColumn, colorN){
  
  require(metafor)
  require(ggplot2)
  
  #1
  list.results<-list()
  list.results1<-list()
  #2
  list.data<-list()
  list.data1<-list()
  #3
  list.figures<-list()
  list.figures1<-list()
  #4
  list.qual<-list()
  list.qual1<-list()
  
  j<-0# TRAITS
  for(j in 1:length(TRAIT)){
    
    i<-0 #MEASCAT
    for(i in 1:length(MEASCAT)){
      
      #subset data by the current effect size measurement and trait value
      dat2<-subset(dat1, measCat==MEASCAT[i] & traitCat==TRAIT[j])
      
      #simplify dataframe
      paperID<-dat2$paperID
      obsID<-dat2$obsID
      xval<-dat2[,PLANT[k]]
      yi<-dat2$yi
      vi<-dat2$vi
      qual<-as.numeric(dat2[,qualColumn])
      data<-data.frame(paperID, obsID, xval, yi, vi, qual)
      data1<-data[!is.na(data$yi) & !is.na(data$vi) & !is.na(data$xval),] #remove NAs for base dataset
      
      #fit a meta-regression with XCAT as x axis
      resPlot<-'convergence failed'
      resPlot0<-'convergence failed'
      resQual<-'convergence failed'
      try(resPlot <- rma.mv(yi, vi, 
                        mods = ~ 1 + xval, 
                        random=list(~1 | paperID, ~1 | obsID), 
                        data=data1, slab=as.character(obsID), method='REML', control=list(maxit=1000)))
      try(resPlot0 <- rma.mv(yi, vi, 
                        mods = ~ 1, 
                        random=list(~1 | paperID, ~1 | obsID), 
                        data=data1, slab=as.character(obsID), method='REML', control=list(maxit=1000)))
      try(resQual <- rma.mv(yi, vi, 
                         mods = ~ 1 + qual, 
                         random=list(~1 | paperID, ~1 | obsID), 
                         data=data1, slab=as.character(obsID), method='REML', control=list(maxit=1000)))
      
      print(paste(j,i))
      
      #res will only be length == 1 if the model did not converge
      if(length(resPlot)==1 | length(resPlot0)==1){
        plantType<-PLANT[k]
        traitType<-TRAIT[j]
        measType<-MEASCAT[i]
        est<-NA
        pVal<-NA
        studies<-dim(data1)[1]
        pseudoR2.1<-NA
        pseudoR2.2<-NA
        pseudoR2.perc<-NA
        result<-data.frame(plantType, traitType, measType, studies, est, pVal, pseudoR2.1, pseudoR2.2, pseudoR2.perc)
      }else{
        result<-SaveFitStats(res=resPlot, res0=resPlot0, k, j, i)
        pred<-predict(resPlot)
        data1$pred<-pred$pred
        data1$ci.lb<-pred$ci.lb
        data1$ci.ub<-pred$ci.ub
      }
      wi<-1/sqrt(data1$vi)
      data1$size<-0.5 + 3.0 * (wi - min(wi))/(max(wi) - min(wi)) #calculate point sizes
      
      
      #1. save the model fit results
      list.results[[i]] <- result
      list.qual[[i]]<-resQual
      
      #2. save the dataset used for plotting
      list.data[[i]] <- data1
      
      #make a plot panel of an effect size against an absolute trait value
      #ylab(paste(ylabs[i], 'Effect Size')) + xlab(paste(globalxlabs1[k], globalxlabs2[j])) +
      data1$qual<-factor(data1$qual)
      colorVals<-grey.colors(n=colorN,start=0.7, end=0)
      
      if(k %in% c(1,2)){
        #if there is a modeladd model fit if needed
        if(length(resPlot)!=1 & length(resPlot0)!=1){
          #if the model is significant
          if(sum(result$pVal<0.1)>0){
            p<-ggplot(data1, aes(x=xval, y=yi, color=qual))  + 
              geom_abline(intercept = 0, slope=0, lty=2) +
              geom_ribbon(aes(x=xval, ymin=ci.lb,ymax=ci.ub),alpha=0.3, fill='blue', inherit.aes = FALSE) +
              #geom_ribbon(aes(x=xval, ymin=cr.lb,ymax=cr.ub),alpha=0.1, fill='blue',inherit.aes = FALSE) +
              geom_line(aes(y=pred), color='black', size=1) + 
              geom_point(shape=19, aes(size=size)) + 
              mytheme + 
              labs(x=NULL, y=NULL) +
              scale_color_manual(values=colorVals) +
              guides(size=FALSE, color=FALSE)
          }else{
            #if the model is not significant
            p<-ggplot(data1, aes(x=xval, y=yi, color=qual)) + 
              geom_abline(intercept = 0, slope=0, lty=2) + 
              geom_point(shape=19, aes(size=size)) + 
              mytheme + 
              labs(x=NULL, y=NULL) +
              scale_color_manual(values=colorVals) +
              guides(size=FALSE, color=FALSE)
          }
        }else{
          #if the model did not converge
          p<-ggplot(data1, aes(x=xval, y=yi, color=qual)) + 
            geom_abline(intercept = 0, slope=0, lty=2) + 
            geom_point(shape=19, aes(size=size)) + 
            mytheme + 
            labs(x=NULL, y=NULL) +
            scale_color_manual(values=colorVals) +
            guides(size=FALSE, color=FALSE)
        }
        
        
      }
      
      if(k %in% c(3,4)){
        
        #if there is a model
        if(length(resPlot)!=1 & length(resPlot0)!=1){
          #if the model is significant
          if(sum(result$pVal<0.1)>0){
            p<-ggplot(data1, aes(x=xval, y=yi, color=qual))  + 
              geom_vline(xintercept = 0, lty=2) +
              geom_abline(intercept = 0, slope=0, lty=2) +
              geom_ribbon(aes(x=xval, ymin=ci.lb,ymax=ci.ub),alpha=0.3, fill='blue', inherit.aes = FALSE) +
              #geom_ribbon(aes(x=xval, ymin=cr.lb,ymax=cr.ub),alpha=0.1, fill='blue',inherit.aes = FALSE) +
              geom_line(aes(y=pred), color='black', size=1) + 
              geom_point(shape=19, aes(size=size)) + 
              mytheme + 
              labs(x=NULL, y=NULL) +
              scale_color_manual(values=colorVals) +
              guides(size=FALSE, color=FALSE)
          }else{
            #if the model is not significant
            p<-ggplot(data1, aes(x=xval, y=yi, color=qual)) + 
              geom_vline(xintercept = 0, lty=2) +
              geom_abline(intercept = 0, slope=0, lty=2) + 
              geom_point(shape=19, aes(size=size)) + 
              mytheme + 
              labs(x=NULL, y=NULL) +
              scale_color_manual(values=colorVals) +
              guides(size=FALSE, color=FALSE)
          }
        }else{
          #if the model did not converge
          p<-ggplot(data1, aes(x=xval, y=yi, color=qual)) + 
            geom_vline(xintercept = 0, lty=2) +
            geom_abline(intercept = 0, slope=0, lty=2) + 
            geom_point(shape=19, aes(size=size)) + 
            mytheme + 
            labs(x=NULL, y=NULL) +
            scale_color_manual(values=colorVals) +
            guides(size=FALSE, color=FALSE)
        }
      }
      
      #3. save the plot panel
      list.figures[[i]]<-p
      
    }
    
    #1
    names(list.results)<-MEASCAT
    list.results1[[j]]<-list.results
    
    #2
    names(list.data)<-MEASCAT
    list.data1[[j]]<-list.data
    
    #3
    names(list.figures)<-MEASCAT
    list.figures1[[j]]<-list.figures
    
    #4
    names(list.qual)<-MEASCAT
    list.qual1[[j]]<-list.qual
    
  }
  
  #1
  names(list.results1)<-TRAIT
  
  #2
  names(list.data1)<-TRAIT
  
  #3
  names(list.figures1)<-TRAIT
  
  #4
  names(list.qual1)<-TRAIT
  
  #Save everything in a big list
  result.list<-list(results=list.results1, data=list.data1, figures=list.figures1, qual=list.qual1)
  
  return(result.list)
}




### Create function to add panel labels to figures objects created by FitPlot ####################################################
AddPanelTitles<-function(LIST){
  
  require(ggplot2)
  
  list.figures<-list()
  list.figures1<-list()
  
  j<-0#TRAITS
  for(j in 1:length(TRAIT)){
    
    i<-0 #MEASCAT
    for(i in 1:length(MEASCAT)){
      
      paneltitle<-paste(labels[i])
      p<-LIST[['figures']][[TRAIT[j]]][[MEASCAT[i]]] + ggtitle(paneltitle)
      
      #3. save the plot panel
      list.figures[[i]]<-p
    }
    names(list.figures)<-MEASCAT
    list.figures1[[j]]<-list.figures
  }
  names(list.figures1)<-TRAIT
  
  return(list.figures1)
}
