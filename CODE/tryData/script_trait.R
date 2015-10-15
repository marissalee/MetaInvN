#tryData/script_trait.R
#Make and clean trait dataset


### Trait dataset ####################################################

#1. Subset the rows containing trait data
dataT<-subset(data, TraitVsNonTrait=='x') #subset the rows containing trait data

#2. Isolate the rows with original ObsDataID (and loose the rows that reference an OrigObsDataID), 
data1<-dataT

# Put these columns back into a format that is easier to manipulate
data1$OrigObsDataID<-as.numeric(data1$OrigObsDataID)
data1$ObsDataID<-as.numeric(data1$ObsDataID)

# Original row is OrigObsDataID == NA or.... 
sum(is.na(data1$OrigObsDataID))

# Original row is OrigObsDataID == ObsDataID
sum(data1$OrigObsDataID == data1$ObsDataID) # 0 rows

# Isolate original rows
data2<-data1[is.na(data1$OrigObsDataID),]
totalrows<-dim(data2)[1]
totalrows

## Are the ObsDataIDs unique? No
uniques<-length(unique(data2$ObsDataID))
uniques
totalrows - uniques # 1,828 duplicates? 

# Clean data and figure out where the duplicates are...

## Get rid of the rows where there is no standardized trait value (StdValue == NA)
sum(is.na(data2$StdValue)) # 38,740 rows without a standardized trait value
data3<-data2[!is.na(data2$StdValue),]
totalrows<-dim(data3)[1]
totalrows

## Are the ObsDataIDs unique now? No
uniques<-length(unique(data3$ObsDataID))
uniques
totalrows - uniques # 1,828 duplicates still.

## Make a list of rows that have a duplicated ObsDataID
ID<-unique(data3$ObsDataID)
dup.list<-numeric(0)
dup.num<-numeric(0)
i<-0
for (i in 1:length(ID)){ #loop through unique ID list
  temp<-data3$ObsDataID==ID[i] # for each ID, see how many rows there are
  if(sum(temp)>1){dup.list<-c(dup.list,ID[i])} #if there is more than one row, then store that ID num in the dup.list vec
  if(sum(temp)>1){dup.num<-c(dup.num,sum(temp))} #if there is more than one row, then store the number of rows in the dup.num vec
}
length(dup.list) # 1,828 duplicates
sum(dup.num==2) # all of them have just 2 rows (no triples)


## Check out a bunch of the duplicate rows
temp<-subset(data3, ObsDataID==dup.list[10]) # checked many duplicate rows and they are in fact duplicates

# Remove duplicate rows
# beware this takes forever
data4<-data3
i<-0
for (i in 1:length(dup.list)){
  temp<-which(data4$ObsDataID==dup.list[i]) # which row number matches dup.list element?
  data4<-data4[-temp[2],] # delete the second element of 'which' from data
}


dataT<-data4
 



