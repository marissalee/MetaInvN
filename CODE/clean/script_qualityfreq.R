#script_qualityfreq.R

### Trait data quality ######################################################
#spIDtraits
# (i) reported in the original study
# (ii) a species-specific database measurement
# (iii) a genus-specific database measurement 

#make spIDtraits a long df
spIDtraits.mean<-melt(spIDtraits, 
                      id.vars=c('spID','GenusSpecies','Genus','Species','obsID'), 
                      measure.vars=c('mean_cn', 'mean_percN','mean_littercn','mean_litterpercN'))
colnames(spIDtraits.mean)[c(6,7)]<-c('traitCat1','traitVal')
spIDtraits.qual<-melt(spIDtraits, 
                      id.vars=c('spID','GenusSpecies','Genus','Species','obsID'), 
                      measure.vars=c('quality_mean_cn', 'quality_mean_percN','quality_mean_littercn','quality_mean_litterpercN'))
colnames(spIDtraits.qual)[c(6,7)]<-c('traitCat2','traitSourceQual')
spIDtraits1<-data.frame(spIDtraits.mean, spIDtraits.qual[,c('traitCat2','traitSourceQual')])
spIDtraits1$traitCat<-NA
spIDtraits1[grepl('cn', spIDtraits1$traitCat1),'traitCat']<-'cn'
spIDtraits1[grepl('percN', spIDtraits1$traitCat1),'traitCat']<-'percN'
spIDtraits1[grepl('cn', spIDtraits1$traitCat1) & grepl('litter', spIDtraits1$traitCat1),'traitCat']<-'littercn'
spIDtraits1[grepl('percN', spIDtraits1$traitCat1) & grepl('litter', spIDtraits1$traitCat1),'traitCat']<-'litterpercN'

#update df structure
spIDtraits1$traitVal<-as.numeric(spIDtraits1$traitVal)
spIDtraits1$traitCat<-factor(spIDtraits1$traitCat, levels = traitCat_order)
spIDtraits1$traitSourceQual<-factor(spIDtraits1$traitSourceQual, levels = c('spID','tryGS','tryGX'))

#plot
pS1c<-ggplot(spIDtraits1, aes(x=traitVal, fill=traitSourceQual)) + 
  geom_histogram() + mytheme +  
  facet_wrap(~traitCat, scales='free') +
  scale_fill_manual(name='Source',
                    label=c("spID" = "Original article",
                            "tryGS" = "Database, species",
                            "tryGX" = "Database, genus"),
                    values = c("spID" = "black",
                               "tryGS" = "blue",
                               "tryGX" = "purple")) +
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + 
  ylab('Count') + xlab('Mean species trait value')
#facet_wrap_labeller(pS1c, labels = prettylabels.tr)



### Cover data quality ######################################################
#spIDcover
# (i) measured
# (ii) BOSD
# (iii) other
# (i2) 1 species cover estimate
# (ii2) >1 species cover estimate

#make spIDcover a long df
spIDcover1<-melt(spIDcover, 
                 id.vars=c('spID','GenusSpecies','Genus','Species','obsID',
                           'cover_qualityMeas','cover_qualityNumSp'), 
                 measure.vars=c('cover_mean_Inv','cover_mean_Nat'))
colnames(spIDcover1)[c(8,9)]<-c('areaCat1','coverVal')
spIDcover1$areaCat<-NA
spIDcover1[grepl('Inv', spIDcover1$areaCat1),'areaCat']<-'InvArea'
spIDcover1[grepl('Nat', spIDcover1$areaCat1),'areaCat']<-'RefArea'

#update df structure
spIDcover1$coverVal<-as.numeric(spIDcover1$coverVal)
spIDcover1$areaCat<-factor(spIDcover1$areaCat, levels = c('InvArea','RefArea'))
spIDcover1$cover_qualityMeas<-factor(spIDcover1$cover_qualityMeas, 
                                     levels = c('measured','BOSD','other'))
spIDcover1$cover_qualityNumSp<-factor(spIDcover1$cover_qualityNumSp, 
                                      levels = c('1 species','>1 species'))

#plot
pS1b<-ggplot(spIDcover1, aes(x=coverVal, fill=cover_qualityMeas, alpha=cover_qualityNumSp)) + 
  geom_histogram() + mytheme +  
  facet_wrap(~areaCat, scales='free', ncol=1) + 
  scale_fill_manual(name='Estimate type',
                    label=c("BOSD" = "Based on study design",
                            "measured" = "Measured",
                            "other" = "Other"),
                    values = c("BOSD" = "blue",
                               "measured" = "black",
                               "other" = "purple")) +
  scale_alpha_manual(name='Spp. disaggregated',
                     label=c("1 species" = "No", 
                             ">1 species" = "Yes"),
                     values = c(">1 species" = 0.5,
                                "1 species" = 1)) + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) + 
  ylab('Count') + xlab('Mean species cover value')
#facet_wrap_labeller(pS1b, labels = c('Invaded Area','Reference Area'))




### CWM data quality ######################################################
#cwm
# (i) cwm value reported in original article
# (ii) cwm value calculated

#update df structure
cwm$qualityCWMcalc<-factor(cwm$qualityCWMcalc, levels = c('reported','calculated'))

pS1a<-ggplot(cwm, aes(x=cwm, fill=qualityCWMcalc)) + 
  facet_wrap(~invType+traitCat, scales='free', ncol=4) + 
  geom_histogram() + mytheme + 
  scale_y_continuous(expand = c(0,0)) + scale_x_continuous(expand = c(0,0)) +
  ylab('Count') + xlab('Community weighted mean (CWM) value')+
  scale_fill_manual(name='Source',
                    label=c("reported" = "Original article","calculated" = "Calculated"),
                    values = c("reported" = "black","calculated" = "blue")) 
prettylabels.trAr<-paste(prettylabels.tr, 
                         rep(c('Invaded Area','Invasive sp.','Reference Area'),
                             each=length(prettylabels.tr)))
#facet_wrap_labeller(pS1a, labels = prettylabels.trAr)


