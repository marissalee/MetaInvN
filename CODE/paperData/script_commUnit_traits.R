#script_commUnit_traits.R
#Assign a common unit for each trait type


# Initialize a table to hold 1 unit for each traitCat
emptyvec<-rep(NA,length(names(traitUnitList)))
df<-data.frame(traitCat=names(traitUnitList), commonUnit=emptyvec)

# sp_cn
traitUnitList[['sp_cn']]
c.unit<-'molC/molN'
df[df$traitCat %in% c('sp_cn'),'commonUnit']<-c.unit

# sp_littercn
traitUnitList[['sp_littercn']]
c.unit<-'molC/molN'
df[df$traitCat %in% c('sp_littercn'),'commonUnit']<-c.unit

# sp_litterpercN
traitUnitList[['sp_litterpercN']]
c.unit<-'%'
df[df$traitCat %in% c('sp_litterpercN'),'commonUnit']<-c.unit

# sp_percN
traitUnitList[['sp_percN']]
c.unit<-'%'
df[df$traitCat %in% c('sp_percN'),'commonUnit']<-c.unit



# REVIEW DECISIONS AND RENAME TABLE
#View(df)
c.unitIndex<-df






