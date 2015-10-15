#######################
#ecosystem
currMeasFac<-anovaMods[anovaMods$ecosystA < 0.1,'.id']
currMeasFac
result<-FactorForest(MeasFac=currMeasFac, res.fac=res.ecosyst, dat.tmp, facCol='ecosystCat')

#update effect size table for plotting
dfr<-ldply(result)
colnames(dfr)[1]<-'MeasFac'
dfr$y<-NA
dfr$CAT<-factor(dfr$CAT, levels=c("forest","shrubland","grassland","wetland","other"))
CATl<-rev(levels(dfr$CAT))
i<-0
for(i in 1:length(CATl)){
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='som','y']<-i #order by CAT
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='soiln','y']<-i+0.15 #dodge by MeasFac1
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='ammonif','y']<-i+0.3 #dodge by MeasFac2
}
unique(dfr$MeasFac)
measlimits<-c("ammonif","soiln","som")
measNames<-c("Ammonif","Soil N","SOM")
shapeVals<-c(0,16,8)
colorVals<-c('burlywood4','burlywood4','burlywood4')

#plot
eco.s<-ggplot(data=dfr,aes(x=estimates,y=y, shape=MeasFac, color=MeasFac, label=ks)) + 
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=cil,xmax=ciu), height=0) +
  geom_vline(xintercept=0,linetype="dashed") + mytheme +
  ggtitle('Ecosystem') + xlab('Std. Mean Diff. (Inv-Ref)') + ylab('') +
  theme(plot.margin=unit(c(0.2,0.01,0.2,0.01),"inches")) +
  scale_y_continuous(breaks=c(1.15,2.15,3.15,4.15,5.15),
                     labels=c('Other',
                              'Wetland',
                              'Grassland',
                              'Shrubland',
                              'Forest')) +
  scale_shape_manual(name="Measurement",labels=measNames,limits=measlimits,values=shapeVals) +
  scale_color_manual(name='Measurement',labels=measNames,limits=measlimits,values=colorVals) +
  geom_text(aes(x=ciu, y=y, hjust=-1, vjust=0.5),size=2.2,show_guide = FALSE)

