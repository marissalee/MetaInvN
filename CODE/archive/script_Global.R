#rmdCode/results/script_Global.R
# Calculate global effect size statistics and plot using the metafor package

library(plyr)
library(metafor)

#create a meta-analysis dataframe
paperID<-tmp$paperID
obsID<-tmp$obsID
quality<-tmp$quality
ecosystCat<-tmp$ecosystCat
studyType<-tmp$studyType
measCat<-tmp$measCat
n1i<-tmp$inv_n
m1i<-tmp$inv_mean_std
sd1i<-sqrt(tmp$inv_var_std)
n2i<-tmp$nat_n
m2i<-tmp$nat_mean_std
sd2i<-sqrt(tmp$nat_var_std)
dat<-data.frame(paperID, obsID, quality, ecosystCat, studyType, measCat, n1i, m1i, sd1i, n2i, m2i, sd2i)

#transform means that have negative values (ammonif, nitrif, nminz)
dat.sub<-subset(dat, measCat %in% c("ammonif", "nitrif","nminz"))
min(dat.sub$m1i, na.rm=T); min(dat.sub$m2i, na.rm=T)
dat.sub$m1i<-dat.sub$m1i + 1
dat.sub$m2i<-dat.sub$m2i + 1
dat[dat$measCat %in% c("ammonif", "nitrif","nminz"),'m1i']<-dat.sub$m1i
dat[dat$measCat %in% c("ammonif", "nitrif","nminz"),'m2i']<-dat.sub$m2i

#replace a 0 with NA
min(dat$m1i, na.rm=T)
min(dat$m2i, na.rm=T)
dat[which(dat$m2i == min(dat$m2i, na.rm=T)),'m2i']<-NA #make this NA instead of 0

#evaluate effect sizes
dat1 <- escalc(measure="ROM", m1i=m1i, sd1i=sd1i, n1i=n1i, m2i=m2i, sd2i=sd2i, n2i=n2i, data=dat) 
#SMD = standardized mean difference
#ROM = log transformed ratio of means 

### fit 3-level random-effects models to some subsets
MEASCAT<-c('nh','no','toti','ammonif','nitrif','nminz','soilmoi','som','soiln','soilcn','biom','litterbiom','percN','cn','litterpercN','littercn')
res.list<-list()
for(i in 1:length(MEASCAT)){
  res <- rma.mv(yi, vi, random=list(~1 | paperID, ~1 | obsID), data=dat1, subset=measCat==MEASCAT[i])
  res.list[[i]]<-res
}
names(res.list)<-MEASCAT

#plot
est.list<-list()
var.list<-list()
i<-0
for(i in 1:length(MEASCAT)){
  est.list[[i]]<-coef(res.list[[as.character(MEASCAT[i])]])
  var.list[[i]]<-vcov(res.list[[as.character(MEASCAT[i])]])
}
estimates<-unlist(est.list)
variances<-unlist(var.list)
labels <- c("Ammonium", "Nitrate", "Total inorganic N", "Ammonification","Nitrification","Mineralization","Soil moisture","Soil organic matter","Soil N","Soil C:N","Plant biomass","Litter biomass","Plant %N","Plant C:N","Litter %N","Litter C:N")
forest(estimates, variances, showweights=F, 
       slab=labels, annotate=F, 
       xlab='Log ratio of means (Inv/Ref)',
       alim=c(-4,3), xlim=c(-4,3))

#View profile likelihood plots
par(mfrow=c(2,1))
i<-0
for(i in 1:length(res.list)){
  res<-res.list[[i]]
  profile(res, sigma2=1)
  mtext(labels[i])
  profile(res, sigma2=2)
  mtext(labels[i])
}
#if both have 1 peak, we can be sure that both variance components are identifiable

#save estimates and pvals
#res.list
i<-0
result.list<-list()
for (i in 1:length(res.list)){
  res<-res.list[[i]]
  p.val<-round(res$pval, 3)
  est<-round(res$b, 3)
  intraPaperCorr<-round(res$sigma2[1] / sum(res$sigma2), 3) #if this # is high, it means that the underlying true effects within paperIDs are estimated to correlate quite strongly
  Heterogen<-round(sum(res$sigma2), 3) #the sum of the two variance components can be interpreted as the total amount of heterogeneity in the true effects
  result.list[[i]]<-c(est, p.val, intraPaperCorr, Heterogen)
}
resultdf<-ldply(result.list, rbind.fill)
colnames(resultdf)<-c("est","pval","intraPaperCorr","heterogen")
resultdf<-cbind(labels, resultdf)
#resultdf
