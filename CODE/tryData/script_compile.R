#tryData/script_compile.R
#Compile raw TRY data


### Coalescing raw data parts 1 and 2 ####################################################

#1. Read-in txt files and rbind them
dataA<-read.table("rawData/TRY_Proposal_117_Data_Release_2012_08_26.txt",header=TRUE,sep="\t") #raw data
dataB<-read.table("rawData/TRY_Proposal_117_Data_Release_2012_09_20.txt",header=TRUE,sep="\t") #raw data
data<-rbind(dataA,dataB)

#2. Make sure that data types are identified correctly
data$ObservationID<-as.factor(data$ObservationID)
data$ObsDataID<-as.factor(data$ObsDataID)
data$TraitID<-as.factor(data$TraitID)
data$DataID<-as.factor(data$DataID)
data$OrigObsDataID<-as.factor(data$OrigObsDataID)

#3. General Info about the dataset/Column headers

#LastName
#FileName
#SpeciesName: original name of the species
#AccSpeciesName: accepted species name (USE THIS)
#ObservationID: identifies which ObsDataIDs are related to one another (USE THIS)
# e.g. two traits measured on the same leaf
# e.g. if plants were grown under experimental conditions, this is reported as a covariate entry with the same ObservationID
#ObsDataID: identifies the row - can be either a trait or a covariate (USE THIS)
#TraitVsNonTrait: identifies rows with trait info (USE THIS)
#TraitID (USE THIS)
#TraitName
#DataID: identifies the trait subgroup (USE THIS)
#DataName
#OrigValueStr: original value as a text string
#OrigUncertaintyStr: original uncertainty as text string
#OrigUnitStr: original unit as text string
#Unit.UnitName:  Unit name
#OrigValue: Original value
#ValueKindName:	Value kind (single measurement, mean, median, etc.)
#StdValue:	Standardized values; not available in all cases (USE THIS)
#Unit_1.UnitName:	Standard unit: Always available for standardized traits (USE THIS)
#RelUncertainty%:	Relative uncertainty in %
#UncertaintyName:	Kind of uncertainty (standard deviation, standard error,...)
#OrigObsDataID:	indicates that that row is a duplicate, contains the ObsDataID of the original entry
#NonTraitCategories:	Type of ancillary data

