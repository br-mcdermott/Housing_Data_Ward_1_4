wd<-getwd()   # put the current directory in wd
Sys.setenv(R_USER=wd) # set R_USER to the working directory
path.expand("~") # now the tilde should point to working directory.



library(dplyr)
#setwd("C:\\Users\\brigid\\Dropbox\\Advocacy Ministry St A\\Data")

source("C:\\Users\\brigid\\Dropbox\\Advocacy Ministry St A\\Data\\1_summary create combined CAMA file for Affordable Housing Database.R")


print(paste("the number of residential units in dataset is",
           sum(cama$NUM_UNITS,na.rm=T),sep=" "))
### estimated number of units in the District



########################  IDENTIFY GOVENMENT,CORPORATE AND COOP OWNERSHIP  ##########

### Identify coop units from USECODE 

cama$coop<-ifelse(cama$USECODE%in%c(26,27,28,126,127),1,0)
sum(cama$NUM_UNITS[cama$coop==1],na.rm=T)###13,642

### Identify government owned buildings using OWNERNAME 
cama$gov_own<-ifelse(cama$OWNERNAME%in%c("DISTRICT OF COLUMBIA HOUSING AUTHORITY", "UNITED STATES OF AMERICA", "DISTRICT OF COLUMBIA", "UNITED STATES OF AMERICA"),
                     1,0)

### identify corporate buildings by specific words in OWNERNAME string
### using method at http://stackoverflow.com/questions/7597559/grep-in-r-with-a-list-of-patterns


to_match<-c("LLC","LP","INC","TRUSTEE","ASSOCIATES","PARTNERSHIP","CORPORATION","TRUSTEES","ASSOCIATION",
            "TRUST","COMPANY","PRTNSHP","LTD","VENTURE","CORP","L.P.","LTP","UNIVERSITY","PROPERTY",
            "PTSP","CORPORTATION","PROPERTIES","LLP","APARTMENTS","PART",
            "FOUNDATION","CHURCH", "L L C", "L.L.C.", "LL","LC", "CONDOMINIUMS","LLL","ASSOCS","ASSOC",
            "STREET","AVENUE","PLACE","REALTY", "L C", "LCC", "PTNRSHP", "ASSC","TRST","TRUST",
            "COOPERATIVE", "COOPERATVE", "ROAD", "AVE" )


#### in using grep I have to make sure I don't pick up
#### the strings in to_match as part of names like
#### "lUIS ROADEM matching "ROAD"

to.match.test<-paste("\\b",to_match,sep="")
to.match.test<-paste(to.match.test, "\\b",sep="")


corporate_rows <- grep(paste(to.match.test,collapse="|"), 
                        cama$OWNERNAME)
cama$corporate_own<-rep(0,nrow(cama))
cama$corporate_own[corporate_rows]<-1
length(cama$corporate_own)


### Identify natural own as those not corporate or govenment owned
cama$natural_own<-ifelse((cama$corporate_own==0 & cama$gov_own==0),
                         1,0)

####################  CALCULATE NUMBER OF UNITS OWNED BY EACH OWNER ######

unit_owner<-aggregate(cama$NUM_UNITS,by=list(cama$OWNERNAME,cama$corporate_own)
                      ,sum,na.rm=T)
names(unit_owner)<-c("OWNERNAME","corporate_own","OWNERUNITS")

### MERGE is by far the fastest way to get ownerunits matched
### many to one match, rows of unit_owner should be repeated for every instance of a 
### particular OWNERNAME

cama.test<-merge(cama,unit_owner,by="OWNERNAME")

stopifnot(nrow(cama.test)==nrow(cama))
cama<-cama.test
rm(cama.test)


####### IDENTIFY UNITS NOT AVAILABLE FOR RENTAL ###

### those units who are likely owner occupied not available for rent
cama$possownocc<-ifelse(
  cama$natural_own==1 & cama$OWNERUNITS==1 & !is.na(cama$OWNERUNITS),1,
  ifelse(!is.na(cama$OWNERUNITS),0,NA))

sum(cama$NUM_UNITS[cama$possownocc==1],na.rm=T)

### in fact we expect that every natual owner lives in a unit whether or not they own
### only one unit or more.  So we expect that 1 unit is unavailable for rent for every
### natural owner in the dataset.

num4rent<-sum(cama$NUM_UNITS,na.rm=T)-length(unique(cama$OWNERNAME[cama$natural_own==1]))
print(paste("the estimated number of units for rent is",num4rent,sep=" "))

### However, since this is a dataset to create a base for identifying individual units
### we will not attempt to weight naturally owned units as likely being owned or 
### as was done with the rent stabilization data.


### marking those units that might be rental
cama$possrent<-ifelse(cama$possownocc==0,1,0)
num.possible.rent<-sum(cama$NUM_UNITS[cama$possrent==1],na.rm=T)


################ MARKING UNITS WHICH MAY BE EXEMPT FROM RENT CONTROL

#### looking at AYB--Actual year built.

#summary(cama$AYB)
#### Correct  obvious typos  
cama$AYB[cama$AYB==19931]<-as.integer(1931)
cama$AYB[cama$AYB==2823]<-as.integer(1955)## from EYB
cama$AYB[cama$AYB==2300]<-as.integer(2000)## guess from google satelite
sum(is.na(cama$AYB))##2
cama$AYB<-ifelse(is.na(cama$AYB),as.integer(cama$EYB),cama$AYB)
### above replaces missing AYB with estimated year built
cama$AYB[cama$AYB<1900]<-as.integer(1954)
### above following lead of codeforDC/rent-stabilization

cama$after1978<-ifelse(cama$AYB>1978,1,0)

sum(cama$NUM_UNITS[cama$after1978==1], na.rm=T)###45,468

######### EXEMPT BECAUSE NATURAL OWNER HAS LESS THAN 5 RENTAL UNITS
#### We are assuming that natual owner has one unit in which they live
cama$less_5<-ifelse(cama$natural_own==1 & cama$OWNERUNITS<6 & !is.na(cama$OWNERUNITS),
                    1,ifelse(!is.na(cama$OWNERUNITS),0,NA))
#sum(cama$NUM_UNITS[cama$less_5==1], na.rm=T)
#sum(cama$NUM_UNITS[cama$possrent==1 & cama$less_5==1],na.rm=T)
##37627

##### USE FUNCTION FROM CODEFORDC/RENT_STABILIZATION

cama$exempt<-ifelse(
    (cama$gov_own==1
    | cama$AYB >= 1978
    | (cama$natural_own==1 & cama$possownocc==0 & cama$OWNERUNITS<= 6)
    | cama$coop==1),
    1,0
    
  ) 


print(paste("number of units that may be exempt from rent stabilization",
            sum(cama$NUM_UNITS[cama$exempt==1], na.rm=T),sep=" "))
print(paste("number of units possibly for rent",
            sum(cama$NUM_UNITS[cama$possrent==1],na.rm=T),
            sep=" "))
