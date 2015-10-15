# For EMAPi abstract

#1st ran script_load.R in 'paperData'


#1) How many papers were detected with each search and how many were accepted/rejected?
summ.papers <- ddply(papers,~source,summarise,
                     numPapers=length(done), numAcceptedPapers=sum(reject=='No', na.rm=T), 
                     numDonePapers=sum(done=='y', na.rm=T), numPapersLeft=sum(reject=='No', na.rm=T)-sum(done=='y', na.rm=T))
orderBy(~-numPapers, summ.papers)
sum(summ.papers$numAcceptedPapers)


#3a) How many accepted papers were published after the most recent Liao2007 reference?  
maxLiaoyr<-max(papers[papers$source == 'Liao2007','year'])
accepted.after<-subset(papers, source != 'Liao2007' & reject == 'No' & year > maxLiaoyr)
paste(dim(accepted.after)[1], 'papers')

#1) How many observations?  
paste(length(unique(observations$obsID)), 'observations')

#2) What is the distribution of observations per paper?  
summ.obs <- ddply(observations,~paperID,summarise, numObs=length(paperID))
range<-paste(range(summ.obs$numObs),collapse=',')
paste('Range of observations per paper =',range , collapse='')
ggplot(summ.obs, aes(x=numObs)) + geom_histogram(binwidth = 1) + theme_classic()

#3) How are observations and papers distributed across ecosystem types?  
summ.obs.eco <- ddply(observations,~ecosystCat,summarise, numObs=length(paperID), numPapers=length(unique(paperID)))
summ.obs.eco

#4) How are observations distrubed across study types?  
summ.obs.st <- ddply(observations,~studyType,summarise, numObs=length(paperID))
summ.obs.st

#5) What is the distribution of invasive species per observation?  Native species?  
summ.spp <- ddply(species,~obsID,summarise, 
                  numTotalspp=length(obsID), 
                  numInvspp=sum(spInvasive=='invasive'), 
                  numNatspp=sum(spInvasive=='native'))

summ.spp

#invasive species
range(summ.spp$numInvspp, na.rm=T)
ggplot(summ.spp, aes(x=numInvspp)) + geom_histogram(binwidth = 1) + theme_classic() 

#native species
range(summ.spp$numNatspp, na.rm=T)
ggplot(summ.spp, aes(x=numNatspp)) + geom_histogram(binwidth = 1) + theme_classic()

#6) Are certain invasive species over-represented?  Distribution of species' presence in observations.  
summ.spp.nam <- ddply(species,~spName+spInvasive,summarise, 
                  numObs=length(obsID), 
                  numPapers=length(unique(paperID)))

#Invasive species that are highly represented
spp.many<-summ.spp.nam[which(summ.spp.nam$numObs > 5 & summ.spp.nam$spInvasive == 'invasive'),] #more than 5 observations
orderBy(~-numObs, spp.many)

#How many observations have cover data?
summ.cov.obs <- ddply(cover,~obsID,summarise, 
                  numMeasured= sum(covQuality=='measured'))
numMeasured.obs<-sum(summ.cov.obs$numMeasured > 0, na.rm=T)
numNot.obs<-length(summ.cov.obs$numMeasured > 0)
cov.obs.perc<-round((numMeasured.obs / (numMeasured.obs+numNot.obs) ) *100, digits=2)
paste(cov.obs.perc, '% of observations with cover data',collapse='')

summ.cov.p <- ddply(cover,~paperID,summarise, 
                  numMeasured= sum(covQuality=='measured'))
numMeasured.p<-sum(summ.cov.p$numMeasured > 0, na.rm=T)
numNot.p<-length(summ.cov.p$numMeasured > 0)
cov.p.perc<-round((numMeasured.p / (numMeasured.p+numNot.p) ) *100, digits=2)
paste(cov.p.perc, '% of papers with cover data',collapse='')

#11) What is the frequency of measurement observations for each measurement type?  
summ.meas <- ddply(measures,~measCat,summarise, 
numMeas = length(obsID),
numObs=length(unique(obsID)))
orderBy(~-numMeas, summ.meas)

#13) What percent of observations and papers have trait data?  
#by observations
n.ot<-length(unique(traits$obsID)) # number of observations with trait data
n.o<-length(unique(observations$obsID)) # total number of observations
tr.obs.perc<-round((n.ot/n.o) *100, digits=2) # percent of observations with trait data
paste(tr.obs.perc, '% of observations with trait data',collapse='')

#by papers
traits$paperID<-as.integer(traits$obsID)
n.pt<-length(unique(traits$paperID)) # number of papers with trait data
n.p<-length(unique(observations$paperID)) # total number of observations
tr.p.perc<-round((n.pt/n.p) *100, digits=2) # percent of papers with trait data
paste(tr.p.perc, '% of papers with trait data',collapse='')

#14) Of the observations that have trait data, what traits are represented and with what frequency?  
summ.tr <- ddply(traits,~traitCat,summarise, 
numObs = length(unique(obsID)), 
numPapers = length(unique(paperID)))
summ.tr #number of unique observations (n.o) and papers (n.p) per trait category 



