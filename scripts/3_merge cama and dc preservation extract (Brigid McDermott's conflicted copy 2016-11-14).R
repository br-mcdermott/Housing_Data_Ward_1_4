### merge,if possible, extract of the DC preservation catalogue with cama file.

wd<-getwd()   # put the current directory in wd
Sys.setenv(R_USER=wd) # set R_USER to the working directory
path.expand("~") # now the tilde should point to working directory.


extract<-read.csv("C:/Users/brigid/Dropbox/consulting/Housing_Data_Ward_1_4/data/dc_pres_catalogue_extract.csv", stringsAsFactors = F,
                  na.strings="[No Value]")
dim(extract)

### the extract was obtained from a simple cut and paste from
### the table at http://dcpres.urban.org/dcp/
### this extract does not have SSL information.  I both entered this
### information 'by hand' by querying the OTR and MAR databases 
insight.extract<-read.csv("C:/Users/brigid/Dropbox/consulting/Housing_Data_Ward_1_4/data/insight_project14.csv", stringsAsFactors = F,
                           na.strings="[No Value]")


### try merging with cama
cama$SSL2<-gsub(" ","",cama$SSL)
extract$SSL2<-gsub(" ","", extract$SSL)

affordable<-merge(extract, cama, by='SSL2')
### 87 entries so 8 entries not matched
### find these and see if you can find matches

affordable<-merge(extract, cama, by='SSL2', all.x=T)
orphan<-affordable[is.na(affordable$SSL.y),]

### correction for portner place which had the wrong SSL
extract$SSL[extract$projectID=="NL000243"]<-"0204 0208"
extract$SSL2<-gsub(" ","", extract$SSL)
### then drop this record from orphan
orphan<-orphan[ -1*(orphan$projectID=="NL000243"),]
###



cama.res$SSL2<-gsub(" ","", cama.res$SSL)
cama.comm$SSL2<-gsub(" ","", cama.comm$SSL)
cama.condo$SSL2<-gsub(" ","", cama.condo$SSL)


### checking not left out of cama file
for(i in 1:7){
res<-cama.res[cama.res$SSL2==orphan$SSL2[i],]
comm<-cama.comm[cama.comm$SSL2==orphan$SSL2[i],]
condo<-cama.condo[cama.condo$SSL2==orphan$SSL2[i],]
print(list(i,res,comm,condo))
}

### no matches for orphan SSL in original cama files

### look for orphan SSL in MAR database and make a new row 
### in the cama file with same SSL and as much information from
###  MAR file that fits with cama file

# basenrow<-nrow(cama)
# mar$SSL2<-gsub(" ","",mar$SSL)
# camaadd<-data.frame(matrix(vector(),nrow(orphan),ncol(cama)), 
#                  stringsAsFactors = F)
# names(camaadd)<-names(cama)
# 
# for(i in 1:nrow(orphan)){
# line<-mar[mar$SSL2==orphan$SSL2[i],]
# if(!is.na(line[1,3])){
#   print(list(i,line))
#   camaadd$OWNERNAME[i]<-orphan[i,"owner"]
#   camaadd$SSL[i]<-line[1,"SSL"]
#   camaadd$USECODE[i]<-NA
#   camaadd$X[i]<-line[1,"LATITUDE"]
#   camaadd$Y[i]<-line[1,"LONGITUDE"]
#   camaadd$BLDG_NUM[i]<-NA
#   camaadd$NUM_UNITS[i]<-line[1,"ACTIVE_RES_UNIT_COUNT"]
#   camaadd$PREMISEADD[i]<-line[1,"FULLADDRESS"]
#   camaadd$AYB[i]<-NA
#   camaadd$EYB[i]<-NA
#   camaadd$UNITNUMBER[i]<-NA
#   camaadd$SALEDATE[i]<-NA
#   camaadd$PRICE[i]<-NA
#   camaadd$LANDAREA[i]<-NA
#   camaadd$X_COORD[i]<-line[1,1]
#   camaadd$Y_COORD[i]<-line[1,2]
#   camaadd$SOURCE[i]<-"MAR"
#   camaadd$SSL2[i]<-gsub(" ","", line[1,"SSL"])
# }
# 
# }
# 
# #### remove rows that have all missing
# 
# camaadd<-camaadd[rowSums(is.na(camaadd)) != ncol(camaadd),]
# ## add records from camaadd to cama
# cama<-rbind(cama,camaadd)

###OK, download otr file and look up SSL  The file is very large so
### use pgAdmin to find the orphan SSL from otr and import as a small
### file

lookups<-read.csv("C:/Users/brigid/Dropbox/consulting/Housing_Data_Ward_1_4/data/orphan_lookups.csv", 
                  stringsAsFactors = F)
lookups$SSL2<-gsub(" ","", lookups$ssl)

camaadd<-data.frame(matrix(vector(),nrow(orphan),ncol(cama)), 
                    stringsAsFactors = F)
names(camaadd)<-names(cama)

for(i in 1:nrow(orphan)){
  line<-lookups[lookups$SSL2==orphan$SSL2[i],]
  if(!is.na(line[1,3])){
    print(list(i,line))
    camaadd$OWNERNAME[i]<-orphan$owner[i]
    camaadd$SSL[i]<-line[1,"ssl"]
    camaadd$USECODE[i]<-line[i,"land_use_code"]
    camaadd$X[i]<-orphan$X[i]
    camaadd$Y[i]<-orphan$Y[i]
    camaadd$BLDG_NUM[i]<-NA
    camaadd$NUM_UNITS[i]<-orphan$Nunits[i]
    camaadd$PREMISEADD[i]<-line[1,"property_address"]
    camaadd$AYB[i]<-NA
    camaadd$EYB[i]<-line[1,"deed_date"]
    camaadd$UNITNUMBER[i]<-NA
    camaadd$SALEDATE[i]<-line[1,"last_sale_date"]
    camaadd$PRICE[i]<-line[1,"last_sale_price"]
    camaadd$LANDAREA[i]<-line[1,"landarea"]
    camaadd$X_COORD[i]<-line[1,1]
    camaadd$Y_COORD[i]<-line[1,2]
    camaadd$SOURCE[i]<-"otr_lookup"
    camaadd$SSL2[i]<-gsub(" ","", line[1,"ssl"])
  }
  
}
#### remove rows that are all missing values from camaadd
camaadd<-camaadd[rowSums(is.na(camaadd)) != ncol(camaadd),]
## add records from camaadd to cama
cama<-rbind(cama,camaadd)

#### now try merge with extract again
affordable<-merge(extract, cama, by='SSL2')
### 87 entries so 8 entries not matched
### find these and see if you can find matches

affordable<-merge(extract, cama, by='SSL2', all.x=T)
orphan<-affordable[is.na(affordable$SSL.y),]

camatest<-merge(cama,extract, by="SSL2", all.x=T)
