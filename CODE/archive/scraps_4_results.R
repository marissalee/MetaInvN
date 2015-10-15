# Set up data to plot relationship between plant trait values and invader impacts
1. Prep plotting parameters
```{r, echo=FALSE, warning=FALSE}
df.tmp<-merge(tmp, cwm, by="obsID")
df.sub<-subset(df.tmp, measCat %in% c("nh","no", "toti","ammonif", "nitrif","nminz","soilmoi","som","soiln","soilcn"))
df.sub <- transform(df.sub, measCat = factor(measCat, levels=c("nh", "no", "toti","ammonif", "nitrif","nminz","soilmoi","som", "soiln","soilcn")))
MEASCAT<-unique(df.sub$measCat) #these will be looped through to make different panels
ylabs <- c("Ammonium", "Nitrate", "Total inorg. N", "Ammonif.","Nitrif.","Minz.","Soil moisture","SOM","Soil N","Soil C:N")
globalylab<-'Standard Mean Difference (Inv-Ref)'
df.sub$traitdiff<-df.sub$inv_mean.y - df.sub$nat_mean.y #calculate the raw mean difference of invaded and native area cwm trait values
esmeas<-"SMD" #to calculate the effect sizes using standard mean differences using the plotting fxn

#"biom","litterbiom","percN","cn","litterpercN","littercn"
#"Plant biomass","Litter biomass","Plant %N","Plant C:N","Litter %N","Litter C:N"
```
2. Organize factors that will be used to color points
a. Quality
```{r,echo=FALSE, warning=FALSE}
# #simplify quality
# df.sub$mqual<-rep(2,length(df.sub$quality)) #mid quality
# df.sub[df.sub$quality=='NoAgg.NoConv', 'mqual']<-3 #high quality
# df.sub[df.sub$quality=='Agg.Conv', 'mqual']<-1 #low quality
# 
# #simplify qualityCover
# df.sub$cqual<-rep(2,length(df.sub$qualityCover))
# df.sub[df.sub$qualityCover=='Measured=NA, 1sp=NA' | df.sub$qualityCover=='Measured=All, 1sp=All', 'cqual']<-3
# df.sub[df.sub$qualityCover=='Measured=None, 1sp=None', 'cqual']<-1
# 
# #simplify qualityTrait
# df.sub$tqual<-rep(2,length(df.sub$qualityTrait))
# df.sub[df.sub$qualityTrait=='Original=NA, SpeciesLevel=NA' | df.sub$qualityCover=='Original=All, SpeciesLevel=All', 'tqual']<-3
# df.sub[df.sub$qualityTrait=='Original=None, SpeciesLevel=None', 'tqual']<-1
# 
# #combine simplified qualities
# df.sub$aqual<-df.sub$mqual + df.sub$cqual + df.sub$tqual
# 
# #create a quality key
# quality.factor<-'aqual'#highest quality are the highest values
# quality.FACTORS<-sort(unique(df.sub[,quality.factor]), decreasing=T)
# quality.COLORS<-heat.colors(length(quality.FACTORS), alpha=.8)
```
b. Study type and Ecosystem
```{r,echo=FALSE, warning=FALSE}
# #create a study type key
# study.factor<-'studyType'
# study.FACTORS<-unique(df.sub[,study.factor])
# study.COLORS<-rainbow(length(study.FACTORS), alpha=.8)
# 
# #create an ecosystem key
# ecosyst.factor<-'ecosystCat'
# ecosyst.FACTORS<-unique(df.sub[,ecosyst.factor])
# ecosyst.COLORS<-rainbow(length(ecosyst.FACTORS), alpha=.8)
```
c. Invasive species
```{r,echo=FALSE, warning=FALSE}
# colnames(species)
# unique(species$spInvasive)
# inv.sp<-subset(species, spInvasive == 'invasive')
# unique(inv.sp$spName) # I want this to just be the genus, so that I can put genus text next to the points

```



3. Plot of invader traits vs invader impacts (role of invader traits)
```{r,echo=FALSE, warning=FALSE, fig.height=8, fig.width=6}
#For all invader traits (x) by invader impacts (y) plots....
esNames<-list(n1='inv_n',m1='inv_mean.x',v1='inv_var',
              n2='nat_n', m2='nat_mean.x', v2='nat_var',
              xvaltype='invINinv_mean')
```
# Invasive species' %N
```{r,echo=FALSE, warning=FALSE, fig.height=8, fig.width=6}
XCAT<-"percN"
xlab<-"Invasive sp. %N"
write.table(df.sub, file="myData.txt", sep="\t")

tmp<-LoopESmeas(df.sub, MEASCAT, ylabs, globalylab, XCAT, xlab, esmeas, esNames)
```