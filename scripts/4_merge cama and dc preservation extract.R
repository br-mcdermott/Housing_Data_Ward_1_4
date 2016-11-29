### merge,if possible, extract of the DC preservation catalogue with cama file.

wd<-getwd()   # put the current directory in wd
Sys.setenv(R_USER=wd) # set R_USER to the working directory
path.expand("~") # now the tilde should point to working directory.


extract<-read.csv("~/data/dc_pres_catalogue_extract.csv", stringsAsFactors = F,
                  na.strings="[No Value]")
dim(extract)

### the extract was obtained from a simple cut and paste from
### the table at http://dcpres.urban.org/dcp/
### this extract does not have SSL information.  I both entered this
### information 'by hand' by querying the OTR and MAR databases 


### try merging with cama14
### create SSL with no spacing to eliminate this as
### a source of error in matching
cama14$SSL2<-gsub(" ","",cama14$SSL)
extract$SSL2<-gsub(" ","", extract$SSL)

affordable<-merge(extract, cama14, by='SSL2')
### 87 entries so 8 entries not matched
### find these and see if you can find matches

affordable<-merge(extract, cama14, by='SSL2', all.x=T)
orphan<-affordable[is.na(affordable$SSL.y),]

### correction for portner place which had the wrong SSL
extract$SSL[extract$projectID=="NL000243"]<-"0204 0208"
extract$SSL2<-gsub(" ","", extract$SSL)
### then drop this record from orphan
###

orphan<-orphan[ -1*(orphan$projectID=="NL000243"),]


cama.res$SSL2<-gsub(" ","", cama.res$SSL)
cama.comm$SSL2<-gsub(" ","", cama.comm$SSL)
cama.condo$SSL2<-gsub(" ","", cama.condo$SSL)


### checking orphan observations not left out of cama file
for(i in 1:7){
res<-cama.res[cama.res$SSL2==orphan$SSL2[i],]  
comm<-cama.comm[cama.comm$SSL2==orphan$SSL2[i],]
condo<-cama.condo[cama.condo$SSL2==orphan$SSL2[i],]
print(list(i,res,comm,condo))
}

### no matches for orphan SSL in original cama files


###OK, download otr file and look up SSL  The file is very large so
### use pgAdmin to find the orphan SSL from otr and import as a small
### file

lookups<-read.csv("~/data/orphan_lookups.csv", 
                  stringsAsFactors = F)
lookups$SSL2<-gsub(" ","", lookups$ssl)

### create empty data frame to put lookup data into
camaadd14<-data.frame(matrix(vector(),nrow(orphan),ncol(cama14)), 
                    stringsAsFactors = F)
names(camaadd14)<-c(names(cama),"SSL2")
### create SSL field with no spaces
camaadd14$SSL2<-gsub(" ","", camaadd14$SSL)

for(i in 1:nrow(orphan)){
  line<-lookups[lookups$SSL2==orphan$SSL2[i],]
   if(!is.na(line[1,3])){
    print(list(i,line))
    camaadd14$OWNERNAME[i]<-orphan$owner[i]
    camaadd14$SSL[i]<-line[1,"ssl"]
    camaadd14$USECODE[i]<-line[i,"land_use_code"]
    camaadd14$X[i]<-orphan$X[i]
    camaadd14$Y[i]<-orphan$Y[i]
    camaadd14$BLDG_NUM[i]<-NA
    camaadd14$NUM_UNITS[i]<-orphan$Nunits[i]
    camaadd14$PREMISEADD[i]<-line[1,"property_address"]
    camaadd14$AYB[i]<-NA
    camaadd14$EYB[i]<-line[1,"deed_date"]
    camaadd14$UNITNUMBER[i]<-NA
    camaadd14$SALEDATE[i]<-line[1,"last_sale_date"]
    camaadd14$PRICE[i]<-line[1,"last_sale_price"]
    camaadd14$LANDAREA[i]<-line[1,"landarea"]
    camaadd14$X_COORD[i]<-line[1,1]
    camaadd14$Y_COORD[i]<-line[1,2]
    camaadd14$SOURCE[i]<-"otr_lookup"
    camaadd14$SSL2[i]<-gsub(" ","", camaadd14$SSL[i])
   }
 }
#### remove rows that are all missing values from camaadd
camaadd14<-camaadd14[rowSums(is.na(camaadd14)) != ncol(camaadd14),]
## add records from camaadd to cama
cama14<-rbind(cama14,camaadd14)

#### now try merge with extract again
affordable<-merge(extract, cama14, by='SSL2')

ward14<-merge(cama14,extract, by="SSL2", all.x=T)
names(ward14)[names(ward14)=="ward.x"]<-"ward"
write.csv(ward14,"~/data/ward14.csv",row.names=F)
write.csv(ward14[ward14$wardx==1,],"~/data/ward1.csv",row.names=F)
write.csv(ward14[ward14$wardx==4,],"~/data/ward4.csv",row.names=F)

nrow(ward14)
length(unique(ward14$SSL2))
####only six non unique SSL2 and those are only repeated once

findrepeat<-aggregate(ward14$SSL.x, by=list(ward14$SSL2), function(x) length(x))
findrepeat<-findrepeat[order(findrepeat[,2], decreasing=T),]
head(findrepeat)
