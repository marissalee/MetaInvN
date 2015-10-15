#tryData/script_nontrait.R
#Clean non-trait dataset


### Non-trait dataset ####################################################

#1. Subset the rows containing non-trait data
dataNT<-subset(data, !TraitVsNonTrait=='x') 


#2. What are the OrigObsDataIDs like? .. all NA
d1<-dataNT

## Put these columns back into a format that is easier to manipulate
d1$OrigObsDataID<-as.numeric(d1$OrigObsDataID)
d1$ObsDataID<-as.numeric(d1$ObsDataID)

dim(d1)[1] # total number of rows
sum(is.na(d1$OrigObsDataID)) # all OrigObsDataID == NA

# Isolate original rows
d2<-d1[is.na(d1$OrigObsDataID),]
totalrows<-dim(d2)[1]
totalrows


## Are the ObsDataIDs unique? No
uniques<-length(unique(d2$ObsDataID))
uniques
totalrows - uniques # 19,297 duplicates? 


## Make a list of rows that have a duplicated ObsDataID
#this took forever to run... be careful.
ID<-unique(d2$ObsDataID)
length(ID) #934,289
dup.list<-numeric(0)
dup.num<-numeric(0)
i<-0
#dup.list 57931:979343
for (i in 1:length(ID)){ #loop through unique ID list
  temp<-d2$ObsDataID==ID[i] # for each ID, see how many rows there are
  print(i)
  print(sum(temp))
  if(sum(temp)>1){dup.list<-c(dup.list,ID[i])} #if there is more than one row, then store that ID num in the dup.list vec
  if(sum(temp)>1){dup.num<-c(dup.num,sum(temp))} #if there is more than one row, then store the number of rows in the dup.num vec
}
length(dup.list) # 1,828 duplicates
sum(dup.num==2) # all of them have just 2 rows (no triples)


## Check out a bunch of the duplicate rows
temp<-subset(d2, ObsDataID==dup.list[100]) # checked many duplicate rows and they are in fact duplicates
temp

# Remove duplicate rows
d3<-d2
i<-0
for (i in 1:length(dup.list)){
  temp<-which(d3$ObsDataID==dup.list[i]) # which row number matches dup.list element?
  d3<-d3[-temp[2],] # delete the second element of 'which' from data
}


dataNT<-d3


