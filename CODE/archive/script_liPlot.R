#legume invader
#######################
currMeasFac<-anovaMods[anovaMods$legInvA < 0.1 & !is.na(anovaMods$legInvA),'.id']
currMeasFac
resultINV<-FactorForest(MeasFac=currMeasFac, res.fac=res.InvLeg, dat.tmp, facCol='InvLeg')

#update effect size table for plotting
dfrINV<-ldply(resultINV)
colnames(dfrINV)[1]<-'MeasFac'
dfrINV$y<-NA
CATl<-rev(levels(dfrINV$CAT))
i<-0
for(i in 1:length(CATl)){
  dfrINV[dfrINV$CAT==CATl[i] & dfrINV$MeasFac=='percN','y']<-i 
  dfrINV[dfrINV$CAT==CATl[i] & dfrINV$MeasFac=='soiln','y']<-i+0.15 #dodge 
  dfrINV[dfrINV$CAT==CATl[i] & dfrINV$MeasFac=='nminz','y']<-i+0.3 #dodge 
}


#legume native percent
#######################
currMeasFac<-anovaMods[anovaMods$legNatA < 0.1 & !is.na(anovaMods$legNatA),'.id']
currMeasFac
resultREF<-FactorForest(MeasFac=currMeasFac, res.fac=res.NatLeg, dat.tmp, facCol='NatLeg')

#update effect size table for plotting
dfrREF<-ldply(resultREF)
colnames(dfrREF)[1]<-'MeasFac'
dfrREF$y<-NA
CATl<-rev(levels(dfrREF$CAT))
i<-0
for(i in 1:length(CATl)){
  dfrREF[dfrREF$CAT==CATl[i] & dfrREF$MeasFac=='percN','y']<-i 
  dfrREF[dfrREF$CAT==CATl[i] & dfrREF$MeasFac=='litterbiom','y']<-i+0.15 #dodge 
  dfrREF[dfrREF$CAT==CATl[i] & dfrREF$MeasFac=='soilcn','y']<-i+0.3 #dodge 
  dfrREF[dfrREF$CAT==CATl[i] & dfrREF$MeasFac=='toti','y']<-i+0.45 #dodge 
}

# #define global legend
# unique(dfrINV$MeasFac); unique(dfrREF$MeasFac)
# measlimits<-c("no","toti",
#               "som",
#               "soiln","soilcn",
#               "litterbiom","percN")
# measNames<-c("Nitrate","Total inorg. N","SOM","Soil N","Soil C:N","Litter biomass","Leaf %N")
# shapeVals<-c(0,1,
#              8,
#              9,10,
#              16,15)
# colorVals<-c('black','black',
#              'black',
#              'black','black',
#              'darkgray','darkgray')


#define separate legends for ALL
unique(dfrINV$MeasFac)
unique(dfrREF$MeasFac)

measlimits.INV<-c("nminz","soiln","percN")
measNames.INV<-c("Mineralization","Soil N", "Leaf %N")
shapeVals.INV<-c(2,8,1)
colorVals.INV<-c('black','black','black')

measlimits.REF<-c("toti","soilcn", "litterbiom","percN")
measNames.REF<-c("Total inorg. N","Soil C:N","Litter biomass","Leaf %N")
shapeVals.REF<-c(0,2,16,15)
colorVals.REF<-c('black','black','black','black')


#plot
#INV plot
li.sp<-ggplot(data=dfrINV,aes(x=estimates,y=y, shape=MeasFac, color=MeasFac, label=ks)) + 
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=cil,xmax=ciu), height=0) +
  geom_vline(xintercept=0,linetype="dashed") + mytheme +
  xlab('Std. Mean Diff. (Inv-Ref)') + ylab('Invasive sp. legumes') +
  theme(plot.margin=unit(c(0.2,0.01,0.2,0.01),"inches")) +
  geom_text(aes(x=ciu, y=y, hjust=-1, vjust=0.5),size=2.2,show_guide = FALSE) +
  scale_y_continuous(breaks=c(1.15,2.15),
                     labels=c('Present','Absent')) +
  scale_shape_manual(name="Measurement",labels=measNames.INV,limits=measlimits.INV,values=shapeVals.INV) +
  scale_color_manual(name='Measurement',labels=measNames.INV,limits=measlimits.INV,values=colorVals.INV)




#REF plot
ln.sp<-ggplot(data=dfrREF,aes(x=estimates,y=y, shape=MeasFac, color=MeasFac, label=ks)) + 
  geom_point(size=3) +
  geom_errorbarh(aes(xmin=cil,xmax=ciu), height=0) +
  geom_vline(xintercept=0,linetype="dashed") + mytheme +
  xlab('Std. Mean Diff. (Inv-Ref)') + ylab('Reference area legumes') +
  theme(plot.margin=unit(c(0.2,0.01,0.2,0.01),"inches")) +
  scale_y_continuous(breaks=c(1.2,2.2),
                     labels=c('Present','Absent'))+
  scale_shape_manual(name="Measurement",labels=measNames.REF,limits=measlimits.REF,values=shapeVals.REF) +
  scale_color_manual(name='Measurement',labels=measNames.REF,limits=measlimits.REF,values=colorVals.REF) +
  geom_text(aes(x=ciu, y=y, hjust=-1, vjust=0.5),size=2.2,show_guide = FALSE)




