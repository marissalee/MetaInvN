#paperData_synthesis/script_commUnit_try.R
#Assign a common unit for each cover measurement


# Initialize a table to hold 1 unit for each covCat
emptyvec<-rep(NA,length(names(unitList)))
df<-data.frame(Cat=names(unitList), commonUnit=emptyvec)

# cn
unitList[['cn']]
c.unit<-'molC/molN'
df[df$Cat %in% c('cn'),'commonUnit']<-c.unit

# littercn
unitList[['littercn']]
c.unit<-'molC/molN'
df[df$Cat %in% c('littercn'),'commonUnit']<-c.unit

# sp_litterpercN
unitList[['litterpercN']]
c.unit<-'%'
df[df$Cat %in% c('litterpercN'),'commonUnit']<-c.unit

# sp_percN
unitList[['percN']]
c.unit<-'%'
df[df$Cat %in% c('percN'),'commonUnit']<-c.unit

# REVIEW DECISIONS AND RENAME TABLE
c.unitIndex<-df
#c.unitIndex






