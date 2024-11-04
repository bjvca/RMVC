### this scipt loads raw data from MCCs and dairy farmers, anonymizes and puts in public folder
rm(list=ls())
library(leaflet)
library(dplyr)

path <- getwd()
path <- strsplit(path,"/data/raw")[[1]]

set.seed(15112024)  #date of creation of this script as random seed

farmers <- read.csv("latest_farmers.csv")


## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid","enumerator","mcc_cas","sub","village","farmer_name","farm_mcc","lat","long","phone1",
             "phone2","map_link","check.Dairy.GPS","check.Dairy._GPS_latitude","check.Dairy._GPS_longitude","check.Dairy._GPS_altitude","check.Dairy._GPS_precision",              
             "meta.instanceID",                              
             "X_id",                                         
             "X_uuid",                                       
             "X_submission_time" ,                           
             "X_date_modified",                              
             "X_tags",                                      
             "X_notes",                                      
             "X_version",                                    
             "X_duration",                                   
             "X_submitted_by",                               
             "X_total_media",                                
             "X_media_count",                                
             "X_media_all_received",                         
             "X_xform_id","check.Dairy.q51_name_other","check.Dairy.q51_name_prev_other","check.Dairy.q51_namex_other","check.Dairy.q51_name_prevx_other","check.Dairy.name_mcc_other")            
farmers <- farmers[ , !(names(farmers) %in% to_drop)]

names(farmers) <- sub("check.Dairy.", "",names(farmers))

### merge in MCC_IDs from listing file
### first rename MCC_ID

names(farmers)[names(farmers) == 'MCC_ID'] <- 'MCC_ID_linked'

MCC_list <- read.csv(paste(path,"questionnaire/list_endline_MCC.csv", sep="/"))[c("name","MCC_ID")]

### replace M

farmers <- merge(farmers,MCC_list,by.x=  "q51_name", by.y="name", all.x=T) 

farmers$MCC_ID[farmers$q51_name == "Other"] <- "MCC_X"
farmers$MCC_ID[farmers$q51_name == "n/a" | farmers$q51_name =="Don’t know"] <- NA

farmers$q51_name <- farmers$MCC_ID
farmers$MCC_ID <- NULL

farmers <- merge(farmers,MCC_list,by.x=  "q51_name_prev", by.y="name", all.x=T) 

farmers$MCC_ID[farmers$q51_name_prev == "Other"] <- "MCC_X"
farmers$MCC_ID[farmers$q51_name_prev == "n/a" | farmers$q51_name_prev =="Don’t know"] <- NA

farmers$q51_name_prev <- farmers$MCC_ID
farmers$MCC_ID <- NULL

farmers <- merge(farmers,MCC_list,by.x=  "q51_namex", by.y="name", all.x=T) 

farmers$MCC_ID[farmers$q51_namex == "Other"] <- "MCC_X"
farmers$MCC_ID[farmers$q51_namex == "n/a" | farmers$q51_namex =="Don’t know"] <- NA

farmers$q51_namex <- farmers$MCC_ID
farmers$MCC_ID <- NULL

farmers <- merge(farmers,MCC_list,by.x=  "q51_name_prevx", by.y="name", all.x=T) 

farmers$MCC_ID[farmers$q51_name_prevx == "Other"] <- "MCC_X"
farmers$MCC_ID[farmers$q51_name_prevx == "n/a" | farmers$q51_name_prevx =="Don’t know"] <- NA

farmers$q51_name_prevx <- farmers$MCC_ID
farmers$MCC_ID <- NULL


farmers <- merge(farmers,MCC_list,by.x=  "name_mcc", by.y="name", all.x=T) 

farmers$MCC_ID[farmers$name_mcc == "Other"] <- "MCC_X"
farmers$MCC_ID[farmers$name_mcc == "n/a" | farmers$name_mcc =="Don’t know"] <- NA

farmers$name_mcc <- farmers$MCC_ID
farmers$MCC_ID <- NULL

##reorder
farmers <- farmers %>%
  select(-c(name_mcc, q51_name_prevx, q51_namex, q51_name_prev, q51_name), 
         everything(), 
         name_mcc, q51_name_prevx, q51_namex, q51_name_prev, q51_name)

write.csv(farmers,paste(path,"data/public/farmers.csv", sep="/"), row.names=FALSE)


###now for MCCs
MCCs <- read.csv("latest_MCCs.csv")

## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid","form_name","enumerator","sub","Village","mcc_name","mcc.gps","mcc._gps_latitude","mcc._gps_longitude","mcc._gps_altitude","mcc._gps_precision",              
             "meta.instanceID",                              
             "X_id",                                         
             "X_uuid",                                       
             "X_submission_time" ,                           
             "X_date_modified",                              
             "X_tags",                                      
             "X_notes",                                      
             "X_version",                                    
             "X_duration",                                   
             "X_submitted_by",                               
             "X_total_media",                                
             "X_media_count",                                
             "X_media_all_received",                         
             "X_xform_id")            
MCCs <- MCCs[ , !(names(MCCs) %in% to_drop)]

write.csv(MCCs,paste(path,"data/public/MCCs.csv", sep="/"), row.names=FALSE)

