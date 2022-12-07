library(leaflet)
library(reshape2)
library(htmlwidgets)

path <- getwd()
MCC <- read.csv("latest_MCC.csv")
farmers <- read.csv("latest_farmers.csv")

gps_MCC <- MCC[,c("MCC_ID","mcc._gps_latitude", "mcc._gps_longitude")]
gps_farmers <- farmers[,c("farmer_ID","check.check2.Dairy._GPS_latitude", "check.check2.Dairy._GPS_longitude")]

gps_MCC$type <- "MCC"
gps_farmers$type <- "farmer"
names(gps_MCC) <- c("ID","latitude","longitude","type")
names(gps_farmers) <- c("ID","latitude","longitude","type")
gps <- rbind(gps_MCC,gps_farmers)

names(gps) <- c("ID","latitude","longitude","type")
gps$latitude <- as.numeric(as.character(gps$latitude))
gps$longitude <- as.numeric(as.character(gps$longitude))
pal <- colorFactor(c("red", "green"),gps$type)
m <- leaflet() %>% setView(lat = mean(gps$latitude, na.rm=T), lng = mean(gps$longitude, na.rm=T), zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=gps, lng=~longitude, lat=~latitude,radius= 2, color=~pal(type), popup = ~as.character(ID) )   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography'))
saveWidget(m, file="dairy_progress.html") 

mean(as.numeric(as.character(farmers$check.check2.Dairy.L2)),na.rm=T) - mean(as.numeric(as.character(farmers$check.check2.Dairy.L1)),na.rm=T)

#how does this compare to the question where the say they used it
prop.table(table(farmers$check.check2.Dairy.Tick3.q103.5[farmers$tick=="TRUE"]))[2]


## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid", "enumerator","q3","q4","parish","village","mcc_name","check.check2.Dairy.q10",
             "check.check2.Dairy.q11", "check.check2.Dairy.q12","check.check2.Dairy.q17","check.check2.Dairy.GPS","check.check2.Dairy._GPS_latitude","check.check2.Dairy._GPS_longitude","check.check2.Dairy._GPS_altitude","check.check2.Dairy._GPS_precision",              
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


## drop location, names and contact details
to_drop <- c("start","end","deviceid","simserial","phonenumber", "subscriberid", "enumerator","district","sub","consent", "mcc.q1","mcc.q2","mcc.q3",
             "mcc.q3a","mcc.q3b","mcc.q3c",
             "mcc.gps","mcc._gps_latitude","mcc._gps_longitude","mcc._gps_altitude","mcc._gps_precision",    
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
MCC <- MCC[ , !(names(MCC) %in% to_drop)]

write.csv(farmers,"/home/bjvca/data/projects/OneCG/RMVC/baseline/data/public/farmers.csv", row.names=FALSE)
write.csv(MCC,"/home/bjvca/data/projects/OneCG/RMVC/baseline/data/public/MCCs.csv", row.names=FALSE)