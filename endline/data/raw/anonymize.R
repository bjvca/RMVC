#### this scipt loads raw data from MCCs and dairy farmers, anonymizes and puts in public folder
rm(list=ls())
library(leaflet)

path <- getwd()
set.seed(15112024)  #date of creation of this script as random seed

farmers <- read.csv("latest_farmers.csv")


## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid","enumerator","sub","village","farmer_name","farm_mcc","lat","long","phone1",
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
             "X_xform_id")            
farmers <- farmers[ , !(names(farmers) %in% to_drop)]

names(farmers) <- sub("check.Dairy.", "",names(farmers))

write.csv(farmers,"/home/bjvca/data/projects/OneCG/RMVC/endline/data/public/farmers.csv", row.names=FALSE)