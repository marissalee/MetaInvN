#Look at qqplots

#Parameters
k<-1


#Loop
j<-0 #TRAITS
for(j in 1:length(TRAIT)){
  
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
    dat2<-subset(dat1, measCat==MEASCAT[i] & traitCat==TRAIT[j])
    
    #simplify dataframe
    paperID<-dat2$paperID
    obsID<-dat2$obsID
    xval<-dat2[,PLANT[k]]
    yi<-dat2$yi
    vi<-dat2$vi
    data<-data.frame(paperID, obsID, xval, yi, vi)
    data1<-data[!is.na(data$yi) & !is.na(data$vi) & !is.na(data$xval),] #remove NAs for base dataset
    
    #plot
    if(dim(data1)[1]>2 & length(unique(data1$xval))!=1){
      
      #qqplot
      #qqplot(x=data1$xval, y=data1$yi)
      #qqnorm(y=data1$yi); qqline(y=data1$yi, col = 2)
      hist(x=data1$yi)
    }
    mtext(ylabs[i], side=2, line=2, cex=.8)
    
    if(dim(data1)[1]<2 & dim(data1)[1] != 0){PlotSparse(xval, yi, size, ylabs)}
    if(sum(dim(data1)[1]) == 0){PlotEmpty()}
    
  }
  mtext(paste(globalxlabs1[k], globalxlabs2[j]), side=1, outer=T) #global xaxis
  mtext(globalylab, side=2, outer=T, line=1.5) #global yaxis
}

