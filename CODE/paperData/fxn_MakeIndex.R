#MakeIndex.R
#Function to make an index for how to convert each unit to the correct one.


### Create function to consolidate index ####################################################
# c.unit<-'ppm/d'
# CAT<-'ammonif'
# charVec<- c('mg/kg*d * (1) ->ppm/d',
#             'ug/g*hr * (24) ->ppm/d',
#             'ug/g*d * (1) ->ppm/d',
#             'ug/g*mo * (1/30) ->ppm/d',
#             'mg/kg*10d * (1/10) ->ppm/d',
#             'umol/g*d * (14.0067/1000000) ->ppm/d',
#             'mg/kg*28d * (1/28) ->ppm/d',
#             'ug/g*30d * (1/30) ->ppm/d')

MakeIndex<-function(CAT, c.unit, charVec){
  
  empty<-''
  
  if(sum(charVec %in% empty)==0){
    #conv.unit
    key2<-ldply(strsplit(charVec, " [*] "), rbind.fill)
    conv.unit<-key2$V1
    
    #multiplier
    key3<-ldply(strsplit(key2$V2,'[] ]'), rbind.fill)
    firstsplit<-ldply(strsplit(key3$V1,"(", fixed=T), rbind.fill)
    multiplier<-as.character(unlist(strsplit(firstsplit$V2, ")", fixed=T)))
    
    #summary
    index1<-data.frame(Cat=rep(CAT, length(conv.unit)),
                       conv.unit, 
                       multiplier, 
                       c.unit=rep(c.unit,length(conv.unit)))
  }
  
  if(sum(charVec %in% empty)==1){
    index1<-data.frame(Cat=CAT,
                       conv.unit=c.unit, 
                       multiplier="1", 
                       c.unit=c.unit)
  }
  
  return(index1)
}





