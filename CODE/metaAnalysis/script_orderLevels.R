#script_orderLevels.R

## Set up the order of measurement types ######################################################

MEASCAT<-c('nh','no','toti',
           'ammonif','nitrif','nminz',
           'soiln','soilcn',
           'som')
labels <- c("Ammonium", "Nitrate", "Total inorganic N", 
            "Ammonification","Nitrification","Mineralization",
            "Soil N","Soil C:N",
            "Soil organic matter")
measTab<-data.frame(MEASCAT,labels)
#measTab



## Do some cleaning for the factor analyses ######################################################

#Quality
dat$InvSpInvArea_qualRank<-factor(dat$InvSpInvArea_qualRank)
dat$NatArea_qualRank<-factor(dat$NatArea_qualRank)
dat$CWMDiff_qualRank<-factor(dat$CWMDiff_qualRank)
dat$CWMDiff2_qualRank<-factor(dat$CWMDiff2_qualRank)

#Ecosystem
#unique(dat$ecosystCat)
dat$ecosystCat <- factor(dat$ecosystCat, 
                         levels = c("forest", 
                                    "shrubland", 
                                    "grassland", 
                                    "wetland",
                                    "other"))

#Study type
#unique(dat$studyType)
dat$studyType <- factor(dat$studyType, 
                        levels = c("field study", 
                                   "field expt addition", 
                                   "field expt removal", 
                                   "greenhouse expt"))

#N-fixer status
#unique(dat$Nfix)
dat$Nfix <- factor(dat$Nfix, 
                  levels = c("No N-fixers",
                             "Resident N-fixers only",
                             "Invasive N-fixers only",
                             "Invasive and resident N-fixers"))



## Set up the order of CWM trait types ######################################################

TRAIT<-c("percN", "litterpercN", "cn", "littercn")
dat$traitCat <- factor(dat$traitCat, levels = TRAIT)
PLANT<-c('InvSpInvArea_cwm', 'NatArea_cwm', 'CWMDiff_cwm','CWMDiff2_cwm')





