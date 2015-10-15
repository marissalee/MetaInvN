#paperData_stdUnits/script_commUnit_meas.R
#Assign a common unit for each measurement


# Initialize a table to hold 1 unit for each measCat
emptyvec<-rep(NA,length(names(measUnitList)))
df<-data.frame(measCat=names(measUnitList), commonUnit=emptyvec)

# inorganic N pools
measUnitList[['nh']]
measUnitList[['no']]
measUnitList[['toti']]
c.unit<-'ppm'
df[df$measCat %in% c('nh','no','toti'),'commonUnit']<-rep(c.unit, 3)

# N fluxes
measUnitList[['ammonif']]
measUnitList[['nitrif']]
measUnitList[['nminz']]
c.unit<-'ppm/d'
df[df$measCat %in% c('ammonif','nitrif','nminz'),'commonUnit']<-rep(c.unit, 3)

# soiln
measUnitList[['soiln']]
c.unit<-'%'
df[df$measCat %in% c('soiln'),'commonUnit']<-rep(c.unit, 1)

# som
measUnitList[['som']]
c.unit<-'%'
df[df$measCat %in% c('som'),'commonUnit']<-rep(c.unit, 1)

# soilmoi
measUnitList[['soilmoi']]
c.unit<-'%'
df[df$measCat %in% c('soilmoi'),'commonUnit']<-rep(c.unit, 1)

# ph
measUnitList[['ph']]
c.unit<-'pH'
df[df$measCat %in% c('ph'),'commonUnit']<-rep(c.unit, 1)

# cn, littercn, soilcn, microbcn
measUnitList[['cn']]
measUnitList[['littercn']]
measUnitList[['soilcn']]
measUnitList[['microbcn']]
c.unit<-'molC/molN'
df[df$measCat %in% c('cn','littercn','soilcn','microbcn'),'commonUnit']<-rep(c.unit, 4)

# percN, litterpercN
measUnitList[['percN']]
measUnitList[['litterpercN']]
c.unit<-'%'
df[df$measCat %in% c('percN','litterpercN'),'commonUnit']<-rep(c.unit, 2)

# plantcov
measUnitList[['plantcov']]
c.unit<-'%'
df[df$measCat %in% c('plantcov'),'commonUnit']<-rep(c.unit, 1)

# biom, litterbiom
measUnitList[['biom']]
measUnitList[['litterbiom']]
c.unit<-'g/m2'
df[df$measCat %in% c('biom','litterbiom'),'commonUnit']<-rep(c.unit, 2)


# REVIEW DECISIONS AND RENAME TABLE
c.unitIndex<-df




