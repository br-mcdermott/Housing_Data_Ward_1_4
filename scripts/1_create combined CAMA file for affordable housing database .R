

### Because of the difficulty is using the ~ tilde character for file path
### extension in different environments the tilde character is manually set 
### to the working directory
wd<-getwd()   # put the current directory in wd
Sys.setenv(R_USER=wd) # set R_USER to the working directory
path.expand("~") # now the tilde should point to working directory.


library(dplyr)

# Computer Assisted Mass Appraisal (CAMA) datasets




#Residential (CAMA) 

download.file("http://opendata.dc.gov/datasets/c5fb3fbe4c694a59a6eef7bf5f8bc49a_25.csv",
             "~/data/cama_residential.csv")

#Conduminium (CAMA) - 48650 x 24

download.file("http://opendata.dc.gov/datasets/d6c70978daa8461992658b69dccb3dbf_24.csv",
             "~/data/cama_condominium.csv")

#Commercial (CAMA) 

download.file("http://opendata.dc.gov/datasets/e53572ef8f124631b965709da8200167_23.csv",
            "~/data/cama_commercial.csv")



## download code for dc rest-stabilization group's use-code file. 
usecodes<-read.csv("~/data/usecodes.csv")



#### read in the cama files.
cama.res <- read.csv('~/data/cama_residential.csv', stringsAsFactors = F)
cama.comm <- read.csv('~/data/cama_commercial.csv', stringsAsFactors = F )
cama.condo <- read.csv('~/data/cama_condominium.csv', stringsAsFactors = F)


#### take only the observations in each property file which have usecodes indicating that
#### they are residential properties.

### take observations from cama datasets which have residential usecodes
cama.res<-cama.res%>%filter(USECODE %in% usecodes$usecode) ###106551 obs
cama.comm<-cama.comm%>%filter(USECODE %in% usecodes$usecode)###8664
cama.condo<-cama.condo%>%filter(USECODE %in% usecodes$usecode)###48650

print(paste("the number of observations cama.res is",
            nrow(cama.res) ,sep=" "))
print(paste("the number of observations cama.comm is",
            nrow(cama.comm) ,sep=" "))
print(paste("the number of observations cama.condo is",
            nrow(cama.condo) ,sep=" "))



### For all buildings in the cama.comm database the combination of SSL
##and BLDG_NUM uniquely identifies a property.


###  From exploration of the data cama.com and from careful reading of the meta data
###  at
###  particularly "Supplemental Information: Most lots have one building
###  in the cama file, assigned BLDG_NUM of one in the table. 
###  For parcels where multiple buildings exist, the primary building 
###  (such as the main residence) is assigned BLDG_NUM = 1. 
###  The other buildings or structures have BLDG_NUM values in random sequential order. 
###  After the primary structure, there is no way to associate BLDG_NUM > 1 records 
###   with any particular structure on the lot."
###   I am assuming that BLDG_NUM=1 is the building with the residential units
###   and that other BLDG_NUM>1  are outbuildings without residential units unless
###   the BLDG_NUM>1 for the same SSL have a different NUM_UNITS or LANDAREA.


#taking only observations from cama.comm where the building number is 1
cama.comm<-filter(cama.comm, BLDG_NUM==1)###8216

print("Taking only BLDG_NUM = 1 for the cama.comm file")

###################################################################################
###  NUM_UNITS an important field in our analysis 
###  Below we attempt to get the best estimate of the number
###  of units that each observation (line in database) represents.
##################################################################################

####NUM_UNITS in cama.res file

### It looks like the number of kitchens would be a good proxy for the number of units
### in the cama.res file
### Below we will replace every missing of NUM_UNITS value in the cama.res table with
### with either 1 if the USECODE indicates it is a 1 unit property or otherwise with the 
### number of kitchens. If the number of kitchens equals 0 then we assume it
### is a studio and set NUM-UNITS equal to 1. The record/observation in which KITCHENS=44
### is corrected to 4



for(i in 1:nrow(cama.res)){
  if(cama.res$KITCHENS[i]==44){cama.res$KITCHENS[i]<-4}### correct single record
  ### next do the following if the NUM_UNITS obs is missing or 0
  if(is.na(cama.res$NUM_UNITS[i])| cama.res$NUM_UNITS[i]==0){
    ## for the usecodes in vector we know the number of units is most likely 1
    if(cama.res$USECODE[i]%in%c(11,12,13,15,16,17,19,116,117,216,217,316,516)){
      cama.res$NUM_UNITS[i]<-1}
    else if(cama.res$KITCHENS[i]==0){
      cama.res$NUM_UNITS[i]<-1}
    else{cama.res$NUM_UNITS[i]<-cama.res$KITCHENS[i]}
  }
}

############### NUM_UNITS IN cama.condo
#### Let us examine the cama.condo file.  Again, we expect most of the observations/records
#### to refer to a single unit. In fact the cama.condo table does not have a NUM_UNITS
####  column/field so we create one.

cama.condo$NUM_UNITS<-rep(1,nrow(cama.condo)) # make a NUM_UNITS variable for cama.condo
                                              # file and a 1 for NUM_UNITS for each obs

### NUM_UNITS FOR THE FILE cama.comm

summary(cama.comm$NUM_UNITS)
#### check record where NUM_UNITS =2660
filter(cama.comm, NUM_UNITS==2660)
#### I phoned this property --they actually have 272 units 
cama.comm$NUM_UNITS[cama.comm$NUM_UNITS==2660]<-272
max(cama.comm$NUM_UNITS, na.rm=T)### check 800

#### Note the many missing (774) NUM_UNITS IN cama.comm.  We will try and use
###  the Master Address Repository (address.point/MAR) to fill in this information

#### Use the Address point file to fill in some of the missing
#### NUM_UNITS

#### detailed use of MAR address point file with cama.commerical to add some of the missing NUM-UNITS
### information.

##### download and read data from address.points data from opendata dc.
download.file("http://opendata.dc.gov/datasets/aa514416aaf74fdc94748f1e56e7cc8a_0.csv", "~/data/mar.csv")

mar<-read.csv("~//data//mar.csv",stringsAsFactors = F)
### this address points table has a field "ACTIVE_RES_OCCUPANCY_COUNT" that gives
## "Number of Housing Units at the primary address" according to the mar data description
## at open data dc.

## Unfortunately MAR does not contain information about
## when the building was built or who the owner is otherwise
## we could just use this datafile

# subset MAR to addresses with at least one housing unit
mar <- mar[mar$ACTIVE_RES_OCCUPANCY_COUNT > 0, ]


#### OUR BIG PROBLEM is the the MAR/address points file has a many to 
#### many relationship with between SSL and address.  We cannot merge
#### on SSL alone. I attempted to create address strings for cama.comm 
#### and mar that would match


### create address in MAR that can be compared to address in cama.commerical
#### 
### need abreviations for street type to match addresses in cama.comm
mar$FULL<-paste(mar$ADDRNUM,mar$STNAME,sep="")
### we have to use abreviations for street type to match cama.comm



#### create a compacted address in the cama.comm table and try matching with MAR


### remove white space from address PREMISEADD  in the cama.comm table.
cama.comm$PREMISEADD1<-as.character(cama.comm$PREMISEADD)
cama.comm$PREMISEADD1<-gsub(" ","",cama.comm$PREMISEADD1)
### need to find and remove all character up to hyphen in address in PREMISEADD
cama.comm$PREMISEADD1<-gsub(".*-","",cama.comm$PREMISEADD1)
### need to remove all trailing Unit information
cama.comm$PREMISEADD1<-gsub("Unit.*","",cama.comm$PREMISEADD1)
### I need to get rid of leading zeros on PREMISEADD
cama.comm$PREMISEADD1<-ifelse(substring(cama.comm$PREMISEADD1,1,1)=="0",
             substring(cama.comm$PREMISEADD1,2), cama.comm$PREMISEADD1)


#### now match the cama.comm with missing NUM_UNITS by the compacted PREMISEADD1 with the 
#### compacted address "FULL" in MAR
test<-filter(cama.comm, (is.na(cama.comm$NUM_UNITS) | cama.comm$NUM_UNITS==0))
join_test<-merge(test,mar,by.x="PREMISEADD1",by.y="FULL")


replace<-rep(0, nrow(join_test))### make a counter for number of 
#### replacement of NUM_UNITS in next code


for(j in 1:nrow(join_test)){
  ### only work with cama.comm where the NUM_UNITS IS MISSING OR 0
   ### following works because OBJECTID is unique id for cama.comm and join_test
       
      if( join_test$SSL.x[j]==join_test$SSL.y[j])
        {  ### ACTIVE_RES_OCCUPANCY_COUNT is field with number of units in mar
           ### if the SSL same find obs in cama.comm that the join_test
           ### was drawn from and replace NUM_UNITS with join_test$ACTIVE_RES_OCCUPANCY_COUNT
        i<-(1:nrow(cama.comm))[cama.comm$OBJECTID==join_test$OBJECTID[j]]
                 cama.comm$NUM_UNITS[i]<-join_test$ACTIVE_RES_OCCUPANCY_COUNT[j]
          replace[j]<-1
       }
     
}
   

print(paste("the number of NUM_UNITS obs. updated from MAR data is",sum(replace),sep=" "))



### NOW SELECT COLUMNS/VARIABLES OF INTEREST AND STACK THE THREE CAMA DATA TABLE TOGETHER.

cama.res$SOURCE<-rep("cama.res",nrow(cama.res))
cama.condo$SOURCE<-rep("cama.condo", nrow(cama.condo))
cama.comm$SOURCE<-rep("cama.comm",nrow(cama.comm))

#### strange character in name for first column of cama tables
names(cama.res)[1]<-"X"
names(cama.condo)[1]<-"X"
names(cama.comm)[1]<-"X"
####### rbind cama data.frames and merge with owner.res
cama.res<-select(cama.res,SSL,USECODE,X,Y,BLDG_NUM,NUM_UNITS,PREMISEADD,AYB,EYB,
                 UNITNUMBER,SALEDATE, PRICE, LANDAREA, OWNERNAME,X_COORD,Y_COORD,SOURCE,OBJECTID)
cama.comm<-select(cama.comm,SSL,USECODE,X,Y,BLDG_NUM,NUM_UNITS,PREMISEADD,AYB,EYB,
                  UNITNUMBER,SALEDATE, PRICE, LANDAREA, OWNERNAME,X_COORD,Y_COORD,SOURCE,OBJECTID)
cama.condo<-select(cama.condo,SSL,USECODE,X,Y,BLDG_NUM,NUM_UNITS,PREMISEADD,AYB,EYB,
                   UNITNUMBER,SALEDATE, PRICE, LANDAREA, OWNERNAME,X_COORD,Y_COORD,SOURCE,OBJECTID)

cama<-rbind(cama.res,cama.comm,cama.condo)

print(paste("the number of observations in the cama file is",
            nrow(cama),sep=" "))

## final substitutions for missing NUM_UNITS

#cama[is.na(cama$NUM_UNITS),]%>%group_by(SOURCE,USECODE)%>%tally
### the largest number of missing NUM_UNITS comes from USECODE 216 and 217

## replace NUM_UNITS with 1 if NUM-UNITS na or 0 and usecode
## indicates NUM_UNITS is 1
just.one<-usecodes$usecode[usecodes$units==1]
cama$NUM_UNITS<-ifelse(is.na(cama$NUM_UNITS) & cama$USECODE%in%just.one| cama$NUM_UNITS==0 & cama$USECODE%in%just.one,1,cama$NUM_UNITS)

print(paste("the number of obs with missing NUM_UNITS is",sum(is.na(cama$NUM_UNITS)),sep=" "))
print(paste("the number of obs. with NUM_UNITS=0 is",sum(cama$NUM_UNITS==0, na.rm=T),sep=" "))
#### OK we have replaced all missing NUM_UNITS that we can for now.

print("remove # on next line to save cama ")
#write.csv(cama,"~/data/cama.csv", row.names=F)