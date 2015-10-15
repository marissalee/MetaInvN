#script_prep1.R

require(metafor)

### Load data ######################################################
observations<-read.table("synthesizedData/clean/observations.txt", header=TRUE, sep="\t") 
measures<-read.table("synthesizedData/clean/measures.txt", header=TRUE, sep="\t")
cwm<-read.table("synthesizedData/clean/cwm.txt", header=TRUE, sep="\t")


### Calculate measurement ESs with either standardized measurement values or non-standardized ######################################################

#study identifiers
obsID<-measures$obsID
measCat<-measures$measCat
measQuality<-measures$YN

#invader impact measures - standardized units
n1i<-measures$inv_n
m1i<-measures$inv_mean_std
sd1i<-sqrt(measures$inv_var_std)
n2i<-measures$nat_n
m2i<-measures$nat_mean_std
sd2i<-sqrt(measures$nat_var_std)

dat.STD<-data.frame(obsID, measCat, measQuality,
                    n1i, m1i, sd1i, n2i, m2i, sd2i)

#invader impact measures - non-standardized units
m1i<-measures$inv_mean
sd1i<-sqrt(measures$inv_var)
m2i<-measures$nat_mean
sd2i<-sqrt(measures$nat_var)

dat.nonSTD<-data.frame(obsID, measCat, measQuality,
                       n1i, m1i, sd1i, n2i, m2i, sd2i)


### Combine the selected measurement ES values with the observation ID modifiers ######################################################
if(chooseMeasType == 'STD'){
  dat<-dat.STD
}
if(chooseMeasType == 'nonSTD'){
  dat<-dat.nonSTD
}

#add obsID factor columns to measures
dat.obs<-merge(dat,observations, by='obsID')
dim(dat);dim(dat.obs) #more columns, same number of rows
colnames(dat.obs) #get rid of unnecessary columns
dat.obs1<-dat.obs[,c('paperID','obsID','measCat',
                 'n1i', 'm1i', 'sd1i', 'n2i', 'm2i', 'sd2i',
                 'measQuality',
                 'ecosystCat','studyType','InvLeg','NatLeg','invGenera')]


### Re-organize cwm data and prep for merging ######################################################
#recast cwm so that type of CWM values are in the same row
cwm.tmp<-cwm[,c('obsID','traitCat','invType','qualRank','cwm')]
m.cwm.tmp<-melt(cwm.tmp, id.vars=c('obsID', 'traitCat','invType'))
c.cwm.tmp<-dcast(m.cwm.tmp, obsID+traitCat~invType+variable)
#View(c.cwm.tmp)

#calculate the raw difference of invaded and native area cwm trait values
c.cwm.tmp$CWMDiff_cwm<-c.cwm.tmp$InvArea_cwm - c.cwm.tmp$NatArea_cwm

### Combine the selected measurement ES values and obsID info with CWM data ######################################################
#add cwm data to measures
dim(dat.obs1); dim(c.cwm.tmp)
dat.all<-merge(dat.obs1, c.cwm.tmp, by='obsID', all=TRUE)
dim(dat.all)
#paste(length(unique(dat.obs1$obsID)), 'observations') #check to make sure that observations did not get dropped
#paste(length(unique(dat.all$obsID)), 'observations')




### Calculate the effect sizes ######################################################
dat.all1 <- escalc(measure=chooseESType, m1i=m1i, sd1i=sd1i, n1i=n1i, m2i=m2i, sd2i=sd2i, n2i=n2i, data=dat.all) 




