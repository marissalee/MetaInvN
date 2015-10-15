#######################
#studyType
currMeasFac<-anovaMods[anovaMods$studytypeA < 0.1,'.id']
currMeasFac
result<-FactorForest(MeasFac=currMeasFac,res.fac=res.studytype, dat.tmp, facCol='studyType')

#update effect size table for plotting
dfr<-ldply(result)
colnames(dfr)[1]<-'MeasFac'
dfr$y<-NA
CATl<-rev(levels(dfr$CAT))
i<-0
for(i in 1:length(CATl)){
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='biom','y']<-i #order by CAT
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='som','y']<-i+0.15 #dodge by MeasFac
  dfr[dfr$CAT==CATl[i] & dfr$MeasFac=='no','y']<-i+0.3 #dodge by MeasFac
}
unique(dfr$MeasFac)
measlimits<-c("no","som","biom")
measNames<-c("Nitrate","SOM","Biomass")
shapeVals<-c(0,15, 16)
colorVals<-c('black','black', 'black')

#plot
stud.s<-ggplot(data=dfr,aes(x=estimates,y=y, shape=MeasFac, color=MeasFac, label=ks)) + 
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=cil,xmax=ciu), height=0) +
  geom_vline(xintercept=0,linetype="dashed") + mytheme +
  xlab('Std. Mean Diff. (Inv-Ref)') + ylab('Study Type') +
  theme(plot.margin=unit(c(0.2,0.01,0.2,0.01),"in")) +
  geom_text(aes(x=ciu, y=y, hjust=-0.5, vjust=0.5),size=3,show_guide = FALSE) +
  scale_y_continuous(breaks=c(1.15, 2.15, 3.15, 4.15),
                     labels=c('Greenhouse\nExpt',
                              'Field Expt\nRemoval',
                              'Field Expt\nAddition',
                              'Field Study'),limits=c(0.65, 4.65))+
  scale_shape_manual(name="Measurement",labels=measNames,limits=measlimits,values=shapeVals) +
  scale_color_manual(name='Measurement',labels=measNames,limits=measlimits,values=colorVals)

  