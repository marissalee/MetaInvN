#quality
#######################
currMeasFac<-anovaMods[anovaMods$qualityA < 0.1,'.id']
currMeasFac
result<-FactorForest(MeasFac=currMeasFac, res.fac=res.quality, dat.tmp=dat.tmp, facCol='measQuality')

#update effect size table for plotting
dfr<-ldply(result)
colnames(dfr)[1]<-'MeasFac'
dfr$y<-NA
CATl<-rev(levels(dfr$CAT))
unique(dfr$MeasFac)

i<-0
for(i in 1:length(CATl)){
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='litterpercN','y']<-i 
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='cn','y']<-i+0.1 #dodge 
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='percN','y']<-i+0.2 #dodge 
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='biom','y']<-i+0.3 #dodge 
  
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='som','y']<-i+0.4
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='nitrif','y']<-i+0.5 #dodge 
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='nminz','y']<-i+0.6 #dodge 
}
unique(dfr$MeasFac)
measlimits<-c("nminz", "nitrif","som","biom","percN","cn","litterpercN")
measNames<-c("Mineralization","Nitrification","SOM","Biomass","Leaf %N","Leaf C:N","Litter %N")
shapeVals<-c(1,15,8,16,0,1,15)
colorVals<-c('burlywood4','burlywood4','burlywood4','darkgreen','darkgreen','darkgreen','darkgreen')

#plot
qual.sp<-ggplot(data=dfr,aes(x=estimates,y=y, shape=MeasFac, color=MeasFac, label=ks)) + 
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=cil,xmax=ciu), height=0) +
  geom_vline(xintercept=0,linetype="dashed") + mytheme +
  ggtitle('Measurement quality') + xlab('Std. Mean Diff. (Inv-Ref)') + ylab('') +
  theme(plot.margin=unit(c(0.2,0.01,0.2,0.01),"inches"))+
  scale_y_continuous(breaks=c(1.35,2.35,3.35,4.35),
                     labels=c('No Manip.',
                              'Units Converted',
                              'Values Aggregated',
                              'Units Converted &\nValues Aggregated')) +
  scale_shape_manual(name="Measurement",labels=measNames,limits=measlimits,values=shapeVals) +
  scale_color_manual(name='Measurement',labels=measNames,limits=measlimits,values=colorVals) +
  geom_text(aes(x=ciu, y=y, hjust=-1, vjust=0.5),size=2.2,show_guide = FALSE)
qual.sp
######################


#update effect size table for plotting
dfr<-ldply(result)
colnames(dfr)[1]<-'MeasFac'
dfr$y<-NA

#CAT
CATl<-rev(levels(dfr$CAT))

#Meas
selectMeas<-unique(dfr$MeasFac)
MeasFac<-selectMeas %in% 
i<-0
for(i in 1:length(MeasFac)){
  dfr[dfr$CAT==CATl[1] & dfr$MeasFac==MeasFac[i],'y']<-i 
  dfr[dfr$CAT==CATl[2] & dfr$MeasFac==MeasFac[i],'y']<-i+0.1 
  dfr[dfr$CAT==CATl[3] & dfr$MeasFac==MeasFac[i],'y']<-i+0.2
  dfr[dfr$CAT==CATl[4] & dfr$MeasFac==MeasFac[i],'y']<-i+0.3 
}
measlimits<-c("nminz", "nitrif","som","biom","percN","cn","litterpercN")
measNames<-c("Mineralization","Nitrification","SOM","Biomass","Leaf %N","Leaf C:N","Litter %N")
shapeVals<-c(1,15,8,16,0,1,15)
colorVals<-c('burlywood4','burlywood4','burlywood4','darkgreen','darkgreen','darkgreen','darkgreen')



#plot
View(dfr)

qual.sp<-ggplot(data=dfr,aes(x=estimates,y=factor(MeasFac), shape=CAT, label=ks)) + 
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=cil,xmax=ciu), height=0)
qual.sp

