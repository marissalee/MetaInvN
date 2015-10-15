#Look at hist

#need to loop through plants because they are different columns in the dataset
k<-0
for(k in 1:length(PLANT)){
  #all traits
  
  #set up plotting area
  layout(matrix(c(1,2,3,
                  4,5,6,
                  7,8,9,
                  10,11,12), nrow=4, ncol=3, byrow = TRUE), 
         widths=1, heights=1)
  par(oma=c(3,3,0,0),mar=c(4,4,0.1,0.1), xpd=FALSE) #bottom, left, top, right
  
  i<-0 #MEASCAT
  for(i in 1:length(MEASCAT)){
    
    #subset data by the current effect size measurement and trait value
    dat2<-subset(dat1, measCat==MEASCAT[i])
    
    #simplify dataframe
    paperID<-dat2$paperID
    obsID<-dat2$obsID
    xval<-dat2[,PLANT[k]]
    yi<-dat2$yi
    vi<-dat2$vi
    data<-data.frame(paperID, obsID, xval, yi, vi)
    data1<-data[!is.na(data$yi) & !is.na(data$vi) & !is.na(data$xval),] #remove NAs for base dataset
    
    #plot
    hist(x=data1$yi, xlab=paste(ylabs[i], 'ES'), main='')
  }
  mtext(paste(globalxlabs1[k]), side=1, outer=T, line=1.5) #global xaxis
}

