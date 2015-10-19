#script_commUnit_cov.R
#Assign a common unit for each cover measurement


# Initialize a table to hold 1 unit for each covCat
emptyvec<-rep(NA,length(names(covUnitList)))
df<-data.frame(covCat=names(covUnitList), commonUnit=emptyvec)

# sp_biomass
covUnitList[['sp_biomass']]
c.unit<-'g/m2'
df[df$covCat %in% c('sp_biomass'),'commonUnit']<-c.unit

# sp_ind
covUnitList[['sp_ind']]
c.unit<-'stems/m2'
df[df$covCat %in% c('sp_ind'),'commonUnit']<-c.unit

# sp_plantcov
covUnitList[['sp_plantcov']]
c.unit<-'%'
df[df$covCat %in% c('sp_plantcov'),'commonUnit']<-c.unit


# REVIEW DECISIONS AND RENAME TABLE
c.unitIndex<-df
#c.unitIndex





