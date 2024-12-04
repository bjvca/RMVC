### this script loads raw data from MCCs, dairy farmers, and samples collected at MCCs, fixes duplicates, anonymizes and puts in public folder
### b.vancampenhout@cgiar.org

rm(list=ls())
library(leaflet)
library(dplyr)

path <- getwd()
path <- strsplit(path,"/data/raw")[[1]]

set.seed(15112024)  #date of creation of this script as random seed

### read in raw farmer data
farmers <- read.csv("latest_farmers.csv")

##duplicates corrections
##pull in baseline to fix duplicates based on gps location
bse <- read.csv("~/data/projects/OneCG/RMVC/endline/questionnaire/list_endline.csv")

dups <- subset(farmers, duplicated(farmers$farmer_ID))
write.csv(dups[c("district","enumerator","farmer_ID","farmer_name","farm_mcc")], file="dups.csv")

### corrections for duplicates go here
##for "F_12363_D" 
farmers$farmer_ID[farmers$X_uuid == "1e98ff1f-a6a2-4905-b319-08b818c0951d"] <- "F_12379_T"
#anything pre_filled should also be replaced (by info from baseline)
farmers[farmers$farmer_ID=="F_12379_T",c("district","sub","MCC_ID","farm_mcc","farmer_name", "farmer_ID", "phone1", "phone2", "lat", "long" , "catchment", "treat", "vid" )] <- bse[bse$farmer_ID == "F_12379_T",c("district", "sub", "MCC_ID","MCC_name" ,"name","farmer_ID","tel_1","tel_2","latitude","longitude", "catchment_ID", "lactoscan", "video")]

##for "F_12363_D" 
farmers$farmer_ID[farmers$X_uuid == "68947581-289b-40c1-993e-ee72a0042eb1"] <- "F_12370_D"
#anything pre_filled should also be replaced (by info from baseline)
farmers[farmers$farmer_ID=="F_12370_D",c("district","sub","MCC_ID","farm_mcc","farmer_name", "farmer_ID", "phone1", "phone2", "lat", "long" , "catchment", "treat", "vid" )] <- bse[bse$farmer_ID == "F_12370_D",c("district", "sub", "MCC_ID","MCC_name" ,"name","farmer_ID","tel_1","tel_2","latitude","longitude", "catchment_ID", "lactoscan", "video")]

farmers$farmer_ID[farmers$X_uuid == "8d3f47d5-155d-4fa7-bff5-e27251fc84d4"] <- "F_4323_T"
farmers[farmers$farmer_ID=="F_4323_T",c("district","sub","MCC_ID","farm_mcc","farmer_name", "farmer_ID", "phone1", "phone2", "lat", "long" , "catchment", "treat", "vid" )] <- bse[bse$farmer_ID == "F_4323_T",c("district", "sub", "MCC_ID","MCC_name" ,"name","farmer_ID","tel_1","tel_2","latitude","longitude", "catchment_ID", "lactoscan", "video")]

farmers$farmer_ID[farmers$X_uuid == "2d0e7c89-bac7-4986-8993-c74f02389dfa"] <- "F_158_D"
farmers[farmers$farmer_ID=="F_158_D",c("district","sub","MCC_ID","farm_mcc","farmer_name", "farmer_ID", "phone1", "phone2", "lat", "long" , "catchment", "treat", "vid" )] <- bse[bse$farmer_ID == "F_158_D",c("district", "sub", "MCC_ID","MCC_name" ,"name","farmer_ID","tel_1","tel_2","latitude","longitude", "catchment_ID", "lactoscan", "video")]

farmers$farmer_ID[farmers$X_uuid == "318a5544-9764-412b-9b14-6d13db64e08f"] <- "F_1271_D"
farmers[farmers$farmer_ID=="F_1271_D",c("district","sub","MCC_ID","farm_mcc","farmer_name", "farmer_ID", "phone1", "phone2", "lat", "long" , "catchment", "treat", "vid" )] <- bse[bse$farmer_ID == "F_1271_D",c("district", "sub", "MCC_ID","MCC_name" ,"name","farmer_ID","tel_1","tel_2","latitude","longitude", "catchment_ID", "lactoscan", "video")]

farmers$farmer_ID[farmers$X_uuid == "670e2a92-528e-4be1-bdbc-a4c838ed7af6"] <- " F_20_T"
farmers[farmers$farmer_ID=="F_20_T",c("district","sub","MCC_ID","farm_mcc","farmer_name", "farmer_ID", "phone1", "phone2", "lat", "long" , "catchment", "treat", "vid" )] <- bse[bse$farmer_ID == "F_20_T",c("district", "sub", "MCC_ID","MCC_name" ,"name","farmer_ID","tel_1","tel_2","latitude","longitude", "catchment_ID", "lactoscan", "video")]

## these could not be identfied or fixed so just drop:
farmers <- subset(farmers, X_uuid != "f14f0c86-f5ac-4cc4-9a14-0a27bfad9f19")
farmers <- subset(farmers, X_uuid != "aec1cce1-0cee-4a9d-aab9-2767138444d5")
farmers <- subset(farmers, X_uuid != "3d0a2b25-b1d8-4e2b-a424-5877ab9231b9")
farmers <- subset(farmers, X_uuid != "fe5e2aef-17ea-4de2-bbfd-9b91559399ad")
farmers <- subset(farmers, X_uuid != "3a6711e2-38ed-4f20-9fe3-53f13a518340")
farmers <- subset(farmers, X_uuid != "0d9710dd-5aba-4974-b0a1-aa83fc43e2b1")
farmers <- subset(farmers, X_uuid != "8e47d2b0-4814-49f6-b3a9-b2ec7280f638")
farmers <- subset(farmers, X_uuid != "8eea5807-bb1e-4d1f-a7f8-9591f096952c")
farmers <- subset(farmers, X_uuid != "959fea38-8eea-4a6b-8cb0-663d7b3b2742")
farmers <- subset(farmers, X_uuid != "f89c31be-20f7-4e1f-b53c-d7567fecf008")
farmers <- subset(farmers, X_uuid != "e74cfa3d-f83d-4aab-8cfc-c26268fa1eeb")
farmers <- subset(farmers, X_uuid != "59c7d912-2ae8-4f0d-927a-c88121f8372d")
farmers <- subset(farmers, X_uuid != "423273fa-0ae3-4a33-8334-fc3ef66f9f9c")
farmers <- subset(farmers, X_uuid != "8daf66db-7c00-41f4-be17-5e7fa44c69bb")
farmers <- subset(farmers, X_uuid != "193a825a-8202-490c-bd4f-64045c7d7092")
farmers <- subset(farmers, X_uuid != "bb0c5319-1425-4e85-baeb-62a8ad6ede98")
farmers <- subset(farmers, X_uuid != "184bd2e8-a29a-45f4-ae47-2e46dc7eba59")
farmers <- subset(farmers, X_uuid != "84f3ff2e-c887-43d9-a978-d2a286cb8375")
farmers <- subset(farmers, X_uuid != "0459651f-f53b-4b60-b233-dd45a3c1e625")
farmers <- subset(farmers, X_uuid != "ced2d819-11bf-4252-b935-928770c7044b")

farmers$farmer_ID[duplicated(farmers$farmer_ID)] #none left

#### this is to investigate duplicated - can be deleted
# vil <- farmers$MCC_ID[farmers$farmer_ID ==  "F_2_T" ][1]
# 
# farmers[farmers$farmer_ID ==  "F_2_T" ,]
# 
# dups <- c("F_2_T")
# missings <- setdiff(bse$farmer_ID[bse$MCC_ID == vil] , farmers$farmer_ID[farmers$MCC_ID == vil])
# non_missings <- intersect( farmers$farmer_ID[farmers$MCC_ID == vil],bse$farmer_ID[bse$MCC_ID == vil] )
# 
# # Baseline missing locations
# test <- bse[bse$farmer_ID %in% missings, c("latitude", "longitude", "farmer_ID")]
# names(test) <- c("lat", "long", "farmer_ID")
# test$category <- "Missing"  # Add category
# 
# # Endline duplicate locations
# test2 <- farmers[farmers$farmer_ID %in% dups, c("check.Dairy._GPS_latitude", "check.Dairy._GPS_longitude", "X_uuid")]
# names(test2) <- c("lat", "long", "farmer_ID")
# test2$category <- "Duplicate"  # Add category
# 
# # Non-missing locations
# test3 <-  bse[bse$farmer_ID %in% non_missings, c("latitude", "longitude", "farmer_ID")]
# names(test3) <- c("lat", "long", "farmer_ID")
# test3$category <- "Covered"  # Add category
# 
# # Combine the datasets
# combined_data <- rbind(test, test2, test3)
# 
# # Assign colors based on category
# combined_data$color <- ifelse(
#   combined_data$category == "Missing", "blue",
#   ifelse(combined_data$category == "Duplicate", "red", "green")
# )
# 
# # Create the map
# m_m <- leaflet(combined_data) %>%
#   setView(lat = 0.65, lng = 33.62, zoom = 11) %>%
#   addTiles(group = "OSM") %>%
#   addTiles(
#     urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",
#     group = "Google",
#     attribution = 'Google'
#   ) %>%
#   addProviderTiles(providers$OpenTopoMap, group = "Topography") %>%
#   addCircleMarkers(
#     lng = ~as.numeric(as.character(long)),
#     lat = ~as.numeric(as.character(lat)),
#     radius = 8,
#     popup = ~paste0("Farmer ID: ", farmer_ID, "<br>Category: ", category),
#     color = ~color
#   ) %>%
#   addLayersControl(baseGroups = c('OSM', 'Google', 'Topography'))
# 
# 

## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid","enumerator","mcc_cas","sub","village","farmer_name","farm_mcc","lat","long","phone1",
             "phone2","map_link","check.Dairy.name_in_app","reas_loc","check.sign","check.Dairy.GPS","check.Dairy._GPS_latitude","check.Dairy._GPS_longitude","check.Dairy._GPS_altitude","check.Dairy._GPS_precision",              
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

### replace MCC names with IDs

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

###get sample list

smpl <- read.csv("~/data/projects/OneCG/RMVC/endline/questionnaire/list_endline_MCC.csv")

missed <- setdiff(smpl$MCC_ID,MCCs$MCC_ID)

write.csv(smpl[smpl$MCC_ID %in% missed,], file = "missed_MCC.csv")

MCCs$MCC_ID[duplicated(MCCs$MCC_ID)] ### MCC 50 is duplicated by design
## this is Nicos MCC that was a replacement for Ibaare (Ntungamo Rubaare_8) that closed at some point, but it opened again so let us keep the last one (and drop nikos)
MCCs <- subset(MCCs, X_uuid != "a342f13a-ca7f-4815-a0c2-06687ffc8a8b")

### we also need to change the district   sub 

## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid","form_name","enumerator","sub","Village","mcc_name","mcc.sign","mcc.gps","mcc._gps_latitude","mcc._gps_longitude","mcc._gps_altitude","mcc._gps_precision",              
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
names(MCCs) <- sub("mcc.", "",names(MCCs))
names(MCCs) <- sub("assets.", "",names(MCCs))
names(MCCs) <- sub("services.", "",names(MCCs))

write.csv(MCCs,paste(path,"data/public/MCCs.csv", sep="/"), row.names=FALSE)


#### now for samples
samples <- read.csv("latest_samples.csv")

to_drop <- c("deviceid","simserial","phonenumber", "subscriberid","form_name","enumerator","sub","Village","mcc_name",
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

samples <- samples[ , !(names(samples) %in% to_drop)]

names(samples) <- sub("mcc.", "",names(samples))
write.csv(samples,paste(path,"data/public/samples.csv", sep="/"), row.names=FALSE)

