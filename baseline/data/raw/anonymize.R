###anonymize baseline data
library(leaflet)
library(reshape2)
library(htmlwidgets)
library(pracma) # for haversine function
library(dplyr)
#set.seed(20042023)  #today's date
set.seed(26092023)  #today's date

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

### look at duplicates for MCCs
sum(duplicated(MCC$MCC_ID))

MCC$MCC_ID[duplicated(MCC$MCC_ID)]
farmers[farmers$q5=="MCC_421", c("mcc_name","X_uuid")]


MCC[MCC$MCC_ID=="MCC_30",c("mcc.q1","X_uuid","mcc._gps_latitude","mcc._gps_longitude")]

#[1] "MCC_152" "MCC_30"  "MCC_291" "MCC_610" "MCC_619"
#[8] "MCC_553" "MCC_487" "MCC_421"
#write.csv(MCC[MCC$MCC_ID %in% MCC$MCC_ID[duplicated(MCC$MCC_ID)],c("MCC_ID","enumerator", "district", "sub", "MCC_ID", "consent", "mcc.q1", "mcc.q2", "mcc.q3", "mcc.q3a", "mcc.q3b", "mcc.q3c")], file="duplicates.csv")
##duplicates were resolved by phone

#Not found
"MCC_612" 


MCC <- subset(MCC,!(X_uuid %in% c("ee42f3a4-f49b-4993-b71e-cf0d8cd9b1ab","8c63c3f8-2e64-491e-8715-d3c0df18745b","290b5dd0-3dc1-4cf5-b6a8-b4a3646bd1e9","c8f7ef54-1fe9-46df-97b7-607d6dd89c2a" )))

MCC$MCC_ID[MCC$X_uuid =="8736079b-0603-4dd2-a817-d94c34220bce"] <- "MCC_216"
MCC$MCC_ID[MCC$X_uuid =="2f142bd7-6552-4458-bd5f-edd0f4abf6f8"] <- "MCC_700"
MCC$MCC_ID[MCC$X_uuid =="4aea383e-0db6-4265-b36e-d382e444a9e9"] <- "MCC_701"
MCC$MCC_ID[MCC$X_uuid =="84e1baeb-cc06-4808-aea8-6f3173c8c220"] <- "MCC_25"
MCC$MCC_ID[MCC$X_uuid =="793dbdcb-c7cc-43f7-a1eb-e7a732253cae"] <- "MCC_91"
MCC$MCC_ID[MCC$X_uuid =="6752168e-8e76-4939-a39d-750a1b9a01a0"] <- "MCC_157"



farmers$q5[farmers$X_uuid=="5f063b31-b2a3-459b-a579-47575ae1de48"] <- "MCC_700"
farmers$q5[farmers$X_uuid=="ef23aaf5-1dfc-4a1d-b3a5-a6855df2229b"] <- "MCC_700"
farmers$q5[farmers$X_uuid=="5d700fe5-81ec-4df1-a6fb-8850c1f73a1b"] <- "MCC_700"
farmers$q5[farmers$X_uuid=="b5ee7c77-534f-4ebd-b993-ed18f91743c0"] <- "MCC_700"
farmers$q5[farmers$X_uuid=="ab268d25-008f-43cb-8bb3-413405f809ad"] <- "MCC_700"
farmers$q5[farmers$X_uuid=="5c2099ce-d107-45a8-85c1-c4fd18d0f05c"] <- "MCC_700"
farmers$q5[farmers$X_uuid=="374d5d9e-8545-45c7-83a4-f65661035fec"] <- "MCC_700"
farmers$q5[farmers$X_uuid=="d8e77d2e-83d6-45c8-9a81-2201d864374b"] <- "MCC_700"

farmers$q5[farmers$X_uuid=="153380a2-ed05-4e7b-87df-abef47000fce"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="bf6dba90-78db-40a3-8d12-fabe80dff0ca"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="8b3f7620-2abf-477e-ad61-d86675703505"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="df9ac241-4973-4683-bc51-86cfd4f0f4fa"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="a698e4fd-112c-4f93-9062-1867954acd3a"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="7facecad-886c-4713-b1ee-ddefffefac5c"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="bc4406b4-87b2-43cf-bfc0-cd558712278d"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="109d4b95-5e2b-4180-9d44-cdd2b12c859b"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="a9ab9845-5e20-47eb-b292-df9ee2520cdd"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="964547b5-d9fd-4de2-9c83-26116cebb579"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="4b120542-c8f9-452b-adb8-006ab882da8e"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="16cb956e-f7f1-4aa8-9453-4a065cd64675"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="163c9c73-fd84-423b-aed6-1efa5c0a767b"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="eaf898ed-d947-43f6-8c81-826eb09e94a5"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="ecc47614-c58a-493b-b742-010828f18551"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="81d38726-c513-45e0-8f86-b868ff255d0a"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="3a781d18-e264-429b-b8ae-2fe8337f1e1a"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="6b1d3ef1-efbd-4c5c-ac30-c920de7f5c1c"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="b57097ce-1254-4bd8-9317-846db2074875"] <- "MCC_701"
farmers$q5[farmers$X_uuid=="c74b58a4-edc3-440f-af6f-94743342efb1"] <- "MCC_701"



### in this part, we categorize MCCs in catchement areas/sheds - 130 MCCs in about 75 cathment areas

MCC$catchment_ID <- NA
counter <- 1

for (mcc_1 in names(table(MCC$MCC_ID))) {
  MCC$catchment_ID[MCC$MCC_ID == mcc_1] <- counter
  for (mcc_2 in names(table(MCC$MCC_ID))) {
    ### key parameter is chosen here: distance to define a catchment area. Here we assume that if shops are less then 2 km apart, they serve the same catchment area
    if ( haversine(c(MCC$mcc._gps_latitude[MCC$MCC_ID == mcc_1] ,MCC$mcc._gps_longitude[MCC$MCC_ID == mcc_1]),c(MCC$mcc._gps_latitude[MCC$MCC_ID == mcc_2] ,MCC$mcc._gps_longitude[MCC$MCC_ID == mcc_2])) < 2) {
      if (is.na(MCC$catchment_ID[MCC$MCC_ID == mcc_2])) {  ## if the shop has not been allocated to a catcchment area yet, create a new one
        MCC$catchment_ID[MCC$MCC_ID == mcc_2] <- counter
      } else {  ## if the shop is already part of a catchment area
        ## change ID of all shops in catchement area to a new catchment area
        MCC$catchment_ID[MCC$catchment_ID == MCC$catchment_ID[MCC$MCC_ID == mcc_1]]  <- MCC$catchment_ID[MCC$MCC_ID == mcc_2] 
      }
      
    }
  }
  counter <- counter + 1
}
dim(table(MCC$catchment_ID))

### make a map with catchment ID coloring and pictures (not public)
pal <- colorFactor(
  palette = 'Dark2',
  domain = MCC$catchment_ID
)


m <- leaflet() %>% setView(lat = -0.6072, lng = 30.654, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=MCC, lng=~mcc._gps_longitude, lat=~mcc._gps_latitude,radius= 8, color=~pal(catchment_ID), popup = ~as.character(catchment_ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 


###

### randomization at catchment area
n_T <- round(length(names(table(MCC$catchment_ID)))/2)
n_C <- length(names(table(MCC$catchment_ID)))-n_T
catchments <- data.frame(cbind(names(table(MCC$catchment_ID)),sample(c(rep("T",n_T), rep("C",n_C))) ))
names(catchments) <- c("catchment_ID","lactoscan")
MCC <- merge(MCC,catchments, by="catchment_ID")

pal <- colorFactor(
  palette = 'Dark2',
  domain = MCC$lactoscan
)

pal <- colorFactor(c("red", "green"),MCC$lactoscan)

m <- leaflet() %>% setView(lat = -0.6072, lng = 30.654, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=MCC, lng=~mcc._gps_longitude, lat=~mcc._gps_latitude,radius= 1, color=~pal(lactoscan), popup = ~as.character(catchment_ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 




### 85 duplicated farmers - just keep the one that is closest to the MCC
farmers$farmer_ID[farmers$X_uuid=="14a4660b-d511-4efc-b874-357149b70162"] <- paste(farmers$farmer_ID[farmers$X_uuid=="14a4660b-d511-4efc-b874-357149b70162"],"R",sep="_" ) 
farmers$farmer_ID[farmers$X_uuid=="d63f3c75-f0be-45c8-b2d2-3782edbd81a2"] <- paste(farmers$farmer_ID[farmers$X_uuid=="d63f3c75-f0be-45c8-b2d2-3782edbd81a2"],"R",sep="_" ) 
farmers$farmer_ID[farmers$X_uuid=="539bfa9a-aeae-4095-9739-bbe342c293f4"] <- paste(farmers$farmer_ID[farmers$X_uuid=="539bfa9a-aeae-4095-9739-bbe342c293f4"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="837863a9-873d-4308-8182-8f555394d9e7"] <- paste(farmers$farmer_ID[farmers$X_uuid=="837863a9-873d-4308-8182-8f555394d9e7"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="a21b3065-54b6-45dc-a449-2b990aec9a86"] <- paste(farmers$farmer_ID[farmers$X_uuid=="a21b3065-54b6-45dc-a449-2b990aec9a86"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="4479eb9c-4612-496c-b95a-30a535385ebe"] <- paste(farmers$farmer_ID[farmers$X_uuid=="4479eb9c-4612-496c-b95a-30a535385ebe"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="9609fad8-67a7-4f79-bc8e-24937123d537"] <- paste(farmers$farmer_ID[farmers$X_uuid=="9609fad8-67a7-4f79-bc8e-24937123d537"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="b49bdc75-f186-4e5b-a7c7-142ad4470cf1"] <- paste(farmers$farmer_ID[farmers$X_uuid=="b49bdc75-f186-4e5b-a7c7-142ad4470cf1"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="38ac377e-92e9-4a47-a458-d89d2b08797b"] <- paste(farmers$farmer_ID[farmers$X_uuid=="38ac377e-92e9-4a47-a458-d89d2b08797b"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="452fedc1-2ef2-4148-803a-6e03e6867d24"] <- paste(farmers$farmer_ID[farmers$X_uuid=="452fedc1-2ef2-4148-803a-6e03e6867d24"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="c07c0bae-02dc-4d9a-99c7-c45db932fd94"] <- paste(farmers$farmer_ID[farmers$X_uuid=="c07c0bae-02dc-4d9a-99c7-c45db932fd94"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="dda88031-347b-4d4c-bf7a-4927394cf6b7"] <- paste(farmers$farmer_ID[farmers$X_uuid=="dda88031-347b-4d4c-bf7a-4927394cf6b7"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid=="77125b33-aeb3-4f5e-922b-7206fd23efa9"] <- paste(farmers$farmer_ID[farmers$X_uuid=="77125b33-aeb3-4f5e-922b-7206fd23efa9"],"R",sep="_" )
farmers$farmer_ID[farmers$X_uuid == "0bd42d23-c15b-4355-8c82-9041ba78aa2a"] <- paste(farmers$farmer_ID[farmers$X_uuid == "0bd42d23-c15b-4355-8c82-9041ba78aa2a"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "e4f4758b-f610-45e6-bc54-6cfdf16279db"] <- paste(farmers$farmer_ID[farmers$X_uuid == "e4f4758b-f610-45e6-bc54-6cfdf16279db"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "2aeea9e6-4c45-4d80-901b-ea3f738de945"] <- paste(farmers$farmer_ID[farmers$X_uuid == "2aeea9e6-4c45-4d80-901b-ea3f738de945"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "49463e8a-2cc6-4a7a-afd4-2e801b6e5a83"] <- paste(farmers$farmer_ID[farmers$X_uuid == "49463e8a-2cc6-4a7a-afd4-2e801b6e5a83"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "57a4b009-12a8-4525-9e1d-4003ddb6f9dd"] <- paste(farmers$farmer_ID[farmers$X_uuid == "57a4b009-12a8-4525-9e1d-4003ddb6f9dd"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "ad5b207c-59f0-438e-be02-1ef57fd46e27"] <- paste(farmers$farmer_ID[farmers$X_uuid == "ad5b207c-59f0-438e-be02-1ef57fd46e27"], "R", sep = "_")

farmers$farmer_ID[farmers$X_uuid=="153380a2-ed05-4e7b-87df-abef47000fce"] <- paste(farmers$farmer_ID[farmers$X_uuid == "153380a2-ed05-4e7b-87df-abef47000fce"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "bf6dba90-78db-40a3-8d12-fabe80dff0ca"] <- paste(farmers$farmer_ID[farmers$X_uuid == "bf6dba90-78db-40a3-8d12-fabe80dff0ca"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "8b3f7620-2abf-477e-ad61-d86675703505"] <- paste(farmers$farmer_ID[farmers$X_uuid == "8b3f7620-2abf-477e-ad61-d86675703505"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "df9ac241-4973-4683-bc51-86cfd4f0f4fa"] <- paste(farmers$farmer_ID[farmers$X_uuid == "df9ac241-4973-4683-bc51-86cfd4f0f4fa"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "a698e4fd-112c-4f93-9062-1867954acd3a"] <- paste(farmers$farmer_ID[farmers$X_uuid == "a698e4fd-112c-4f93-9062-1867954acd3a"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "7facecad-886c-4713-b1ee-ddefffefac5c"] <- paste(farmers$farmer_ID[farmers$X_uuid == "7facecad-886c-4713-b1ee-ddefffefac5c"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "bc4406b4-87b2-43cf-bfc0-cd558712278d"] <- paste(farmers$farmer_ID[farmers$X_uuid == "bc4406b4-87b2-43cf-bfc0-cd558712278d"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "109d4b95-5e2b-4180-9d44-cdd2b12c859b"] <- paste(farmers$farmer_ID[farmers$X_uuid == "109d4b95-5e2b-4180-9d44-cdd2b12c859b"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "a9ab9845-5e20-47eb-b292-df9ee2520cdd"] <- paste(farmers$farmer_ID[farmers$X_uuid == "a9ab9845-5e20-47eb-b292-df9ee2520cdd"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "964547b5-d9fd-4de2-9c83-26116cebb579"] <- paste(farmers$farmer_ID[farmers$X_uuid == "964547b5-d9fd-4de2-9c83-26116cebb579"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "4b120542-c8f9-452b-adb8-006ab882da8e"] <- paste(farmers$farmer_ID[farmers$X_uuid == "4b120542-c8f9-452b-adb8-006ab882da8e"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "16cb956e-f7f1-4aa8-9453-4a065cd64675"] <- paste(farmers$farmer_ID[farmers$X_uuid == "16cb956e-f7f1-4aa8-9453-4a065cd64675"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "163c9c73-fd84-423b-aed6-1efa5c0a767b"] <- paste(farmers$farmer_ID[farmers$X_uuid == "163c9c73-fd84-423b-aed6-1efa5c0a767b"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "eaf898ed-d947-43f6-8c81-826eb09e94a5"] <- paste(farmers$farmer_ID[farmers$X_uuid == "eaf898ed-d947-43f6-8c81-826eb09e94a5"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "ecc47614-c58a-493b-b742-010828f18551"] <- paste(farmers$farmer_ID[farmers$X_uuid == "ecc47614-c58a-493b-b742-010828f18551"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "81d38726-c513-45e0-8f86-b868ff255d0a"] <- paste(farmers$farmer_ID[farmers$X_uuid == "81d38726-c513-45e0-8f86-b868ff255d0a"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "3a781d18-e264-429b-b8ae-2fe8337f1e1a"] <- paste(farmers$farmer_ID[farmers$X_uuid == "3a781d18-e264-429b-b8ae-2fe8337f1e1a"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "6b1d3ef1-efbd-4c5c-ac30-c920de7f5c1c"] <- paste(farmers$farmer_ID[farmers$X_uuid == "6b1d3ef1-efbd-4c5c-ac30-c920de7f5c1c"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "b57097ce-1254-4bd8-9317-846db2074875"] <- paste(farmers$farmer_ID[farmers$X_uuid == "b57097ce-1254-4bd8-9317-846db2074875"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "c74b58a4-edc3-440f-af6f-94743342efb1"] <- paste(farmers$farmer_ID[farmers$X_uuid == "c74b58a4-edc3-440f-af6f-94743342efb1"], "R", sep = "_")

farmers$farmer_ID[farmers$X_uuid == "2c92f150-6a1c-4163-bae7-61894e459743"] <- paste(farmers$farmer_ID[farmers$X_uuid == "2c92f150-6a1c-4163-bae7-61894e459743"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "794a0503-96e2-4913-9561-69f64f5b9a07"] <- paste(farmers$farmer_ID[farmers$X_uuid == "794a0503-96e2-4913-9561-69f64f5b9a07"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "61812749-2376-4587-ba7d-6a481abce838"] <- paste(farmers$farmer_ID[farmers$X_uuid == "61812749-2376-4587-ba7d-6a481abce838"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "a714585b-ea17-4a52-b7d9-07d9004f48f7"] <- paste(farmers$farmer_ID[farmers$X_uuid == "a714585b-ea17-4a52-b7d9-07d9004f48f7"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "f5a2f8a1-0cb7-4364-83bc-d5a44368f1c9"] <- paste(farmers$farmer_ID[farmers$X_uuid == "f5a2f8a1-0cb7-4364-83bc-d5a44368f1c9"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "4eadfe7b-5629-441b-93bc-858b5f49d2d6"] <- paste(farmers$farmer_ID[farmers$X_uuid == "4eadfe7b-5629-441b-93bc-858b5f49d2d6"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "03651b7e-54fe-4d33-a9b4-2b686e67cf2a"] <- paste(farmers$farmer_ID[farmers$X_uuid == "03651b7e-54fe-4d33-a9b4-2b686e67cf2a"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "d1743ef4-a3ee-4733-966f-05f030169d8c"] <- paste(farmers$farmer_ID[farmers$X_uuid == "d1743ef4-a3ee-4733-966f-05f030169d8c"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "746ed66d-1c9b-4d07-a508-378dfb3f01ef"] <- paste(farmers$farmer_ID[farmers$X_uuid == "746ed66d-1c9b-4d07-a508-378dfb3f01ef"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "27546a61-9fb6-46d6-8ad9-e1befc39d1af"] <- paste(farmers$farmer_ID[farmers$X_uuid == "27546a61-9fb6-46d6-8ad9-e1befc39d1af"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "1a4b13da-31d9-4f6a-8b17-554a4d49fa0b"] <- paste(farmers$farmer_ID[farmers$X_uuid == "1a4b13da-31d9-4f6a-8b17-554a4d49fa0b"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "5e92fa5a-a1ee-42eb-ace2-a1bb2092fb01"] <- paste(farmers$farmer_ID[farmers$X_uuid == "5e92fa5a-a1ee-42eb-ace2-a1bb2092fb01"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "a29076ee-1a58-427f-b958-e9cd85262058"] <- paste(farmers$farmer_ID[farmers$X_uuid == "a29076ee-1a58-427f-b958-e9cd85262058"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "d620417c-ea2a-48a8-a838-d4a9259995ff"] <- paste(farmers$farmer_ID[farmers$X_uuid == "d620417c-ea2a-48a8-a838-d4a9259995ff"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "a8a084f8-d46a-4644-acfd-f3a115e431e2"] <- paste(farmers$farmer_ID[farmers$X_uuid == "a8a084f8-d46a-4644-acfd-f3a115e431e2"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "5e0cb196-0ccb-4691-9a89-537afea68178"] <- paste(farmers$farmer_ID[farmers$X_uuid == "5e0cb196-0ccb-4691-9a89-537afea68178"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "efe8e82a-dda3-4b02-a72c-b3033c7e2e29"] <- paste(farmers$farmer_ID[farmers$X_uuid == "efe8e82a-dda3-4b02-a72c-b3033c7e2e29"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "47b58b64-cc77-41a5-a86e-4339151968d8"] <- paste(farmers$farmer_ID[farmers$X_uuid == "47b58b64-cc77-41a5-a86e-4339151968d8"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "1994811d-2129-46a6-bf51-f26e903ff5ed"] <- paste(farmers$farmer_ID[farmers$X_uuid == "1994811d-2129-46a6-bf51-f26e903ff5ed"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "e83169c8-e6f1-477e-b73c-806a135fdc93"] <- paste(farmers$farmer_ID[farmers$X_uuid == "e83169c8-e6f1-477e-b73c-806a135fdc93"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "fc424a15-1f6c-4060-a7ba-88e44ea39bc3"] <- paste(farmers$farmer_ID[farmers$X_uuid == "fc424a15-1f6c-4060-a7ba-88e44ea39bc3"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "ed32da23-907b-4147-af91-096892b6e672"] <- paste(farmers$farmer_ID[farmers$X_uuid == "ed32da23-907b-4147-af91-096892b6e672"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "31d24908-a603-461b-86da-d85b1ed7e7d3"] <- paste(farmers$farmer_ID[farmers$X_uuid == "31d24908-a603-461b-86da-d85b1ed7e7d3"], "R", sep = "_")
#farmers$farmer_ID[farmers$X_uuid == "71335f3f-ad2b-48d6-ab95-27c600fa5f42"] <- paste(farmers$farmer_ID[farmers$X_uuid == "71335f3f-ad2b-48d6-ab95-27c600fa5f42"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "3e20b5a6-d635-4fd2-bda9-4c5154e4de07"] <- paste(farmers$farmer_ID[farmers$X_uuid == "3e20b5a6-d635-4fd2-bda9-4c5154e4de07"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "f1692b8a-04ab-4b91-885c-388607d9789d"] <- paste(farmers$farmer_ID[farmers$X_uuid == "f1692b8a-04ab-4b91-885c-388607d9789d"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "4b4c62fb-c860-4b60-976e-9860e0378574"] <- paste(farmers$farmer_ID[farmers$X_uuid == "4b4c62fb-c860-4b60-976e-9860e0378574"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "446656a5-f92a-48fe-b80d-a737b41a7333"] <- paste(farmers$farmer_ID[farmers$X_uuid == "446656a5-f92a-48fe-b80d-a737b41a7333"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "6752bcd3-42bf-47e8-95ed-fdc29535f828"] <- paste(farmers$farmer_ID[farmers$X_uuid == "6752bcd3-42bf-47e8-95ed-fdc29535f828"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid == "976c4a8d-d705-4212-a6fd-f9c1c729a206"] <- paste(farmers$farmer_ID[farmers$X_uuid == "976c4a8d-d705-4212-a6fd-f9c1c729a206"], "R", sep = "_")

farmers$farmer_ID[farmers$X_uuid=="5f063b31-b2a3-459b-a579-47575ae1de48"] <- paste(farmers$farmer_ID[farmers$X_uuid == "5f063b31-b2a3-459b-a579-47575ae1de48"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="ef23aaf5-1dfc-4a1d-b3a5-a6855df2229b"] <- paste(farmers$farmer_ID[farmers$X_uuid == "ef23aaf5-1dfc-4a1d-b3a5-a6855df2229b"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="5d700fe5-81ec-4df1-a6fb-8850c1f73a1b"] <- paste(farmers$farmer_ID[farmers$X_uuid == "5d700fe5-81ec-4df1-a6fb-8850c1f73a1b"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="b5ee7c77-534f-4ebd-b993-ed18f91743c0"] <- paste(farmers$farmer_ID[farmers$X_uuid == "b5ee7c77-534f-4ebd-b993-ed18f91743c0"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="ab268d25-008f-43cb-8bb3-413405f809ad"] <- paste(farmers$farmer_ID[farmers$X_uuid == "ab268d25-008f-43cb-8bb3-413405f809ad"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="5c2099ce-d107-45a8-85c1-c4fd18d0f05c"] <- paste(farmers$farmer_ID[farmers$X_uuid == "5c2099ce-d107-45a8-85c1-c4fd18d0f05c"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="374d5d9e-8545-45c7-83a4-f65661035fec"] <- paste(farmers$farmer_ID[farmers$X_uuid == "374d5d9e-8545-45c7-83a4-f65661035fec"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="d8e77d2e-83d6-45c8-9a81-2201d864374b"] <- paste(farmers$farmer_ID[farmers$X_uuid == "d8e77d2e-83d6-45c8-9a81-2201d864374b"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="09ebf6a2-bc8b-4adf-837c-81a34066e73e"] <- paste(farmers$farmer_ID[farmers$X_uuid == "09ebf6a2-bc8b-4adf-837c-81a34066e73e"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="25fdacaa-509c-474b-98d6-ab69e2fe04f1"] <- paste(farmers$farmer_ID[farmers$X_uuid == "25fdacaa-509c-474b-98d6-ab69e2fe04f1"], "R", sep = "_")

farmers$farmer_ID[farmers$X_uuid=="9e9ffbe8-0d53-4d93-aaa5-59a7134d8d24"] <- paste(farmers$farmer_ID[farmers$X_uuid == "9e9ffbe8-0d53-4d93-aaa5-59a7134d8d24"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="4db51899-ef72-4ea5-b32c-4df7c4de1be2"] <- paste(farmers$farmer_ID[farmers$X_uuid == "4db51899-ef72-4ea5-b32c-4df7c4de1be2"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="8fe74d5b-a0c9-4c72-85fb-4e669f517337"] <- paste(farmers$farmer_ID[farmers$X_uuid == "8fe74d5b-a0c9-4c72-85fb-4e669f517337"], "R", sep = "_")
farmers$farmer_ID[farmers$X_uuid=="5c78d865-a3cc-4579-8288-297555d6ba6c"] <- paste(farmers$farmer_ID[farmers$X_uuid == "5c78d865-a3cc-4579-8288-297555d6ba6c"], "R", sep = "_")




farmers <- subset(farmers,X_uuid!="e8846c53-8fc0-4486-a92e-0e34edc602a8")
farmers <- subset(farmers,X_uuid!="4162927e-bc44-41da-99ff-05f1449166bd")
farmers <- subset(farmers,X_uuid!="00fd2dff-cbc7-4737-b7d9-6fd50e82034c")
farmers <- subset(farmers,X_uuid!="8d16d853-b619-413d-91c0-6ecf0dee8528")
farmers <- subset(farmers,X_uuid!="6e0ca483-33e3-4f6a-ac4c-c40d6c37d099")
farmers <- subset(farmers,X_uuid!="839171fe-ae9b-4c59-bc1e-c495fb9eaaca")
farmers <- subset(farmers,X_uuid!="b5ee7c77-534f-4ebd-b993-ed18f91743c0")
farmers <- subset(farmers,X_uuid!="d8e77d2e-83d6-45c8-9a81-2201d864374b")
farmers <- subset(farmers,X_uuid!="71335f3f-ad2b-48d6-ab95-27c600fa5f42")
farmers <- subset(farmers,X_uuid!="40989731-3a12-4878-ac0a-1af8696d47f7")
farmers <- subset(farmers,X_uuid!="553f938e-5aa4-40e6-b74c-2c03d787e0fe")
farmers <- subset(farmers,X_uuid!="04c555c4-e1d5-46f0-bb15-bfc434a7b725")

farmers$q5[farmers$q5=="MCC_28"] <- "MCC_227" 
farmers$q5[farmers$q5=="MCC_608"] <- "MCC_614" 

farmers <- subset(farmers, !(farmers$q5 %in% c("MCC_214","MCC_284","MCC_292","MCC_627")))

MCC <- subset(MCC, !(MCC$MCC_ID %in% c("MCC_16","MCC_215")))

sum(duplicated(farmers$farmer_ID))

farmers$farmer_ID[duplicated(farmers$farmer_ID)] 

ID <- "F_2710_T"

farmers[farmers$farmer_ID==ID,c("check.check2.Dairy._GPS_latitude", "check.check2.Dairy._GPS_longitude")]

trial <- data.frame("Dup",farmers[farmers$farmer_ID==ID,c("X_uuid","check.check2.Dairy._GPS_latitude", "check.check2.Dairy._GPS_longitude")])
names(trial) <- c("type","ID","lat","long")
trial2 <- data.frame("MCC",c("MCC",MCC[MCC$MCC_ID=="MCC_77",c("mcc._gps_latitude","mcc._gps_longitude")]))
names(trial2) <- c("type","ID","lat","long")

trial3 <- data.frame("farmers",c(farmers[farmers$q5=="MCC_77",c("X_uuid","check.check2.Dairy._GPS_latitude", "check.check2.Dairy._GPS_longitude")]))
names(trial3) <- c("type","ID","lat","long")

trial <- rbind(trial,trial2,trial3)

pal <- colorFactor(
  palette = 'Dark2',
  domain = trial$type
)
m <- leaflet() %>% setView(lat = -0.6072, lng = 30.654, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=trial, lng=~as.numeric(long), lat=~as.numeric(lat),radius= 8, color=~pal(type), popup = ~as.character(ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

### make a map with

setdiff(names(table(MCC$MCC_ID)), names(table(farmers$q5)))

setdiff( names(table(farmers$q5)), names(table(MCC$MCC_ID)))

farmers <- merge(farmers, MCC[c("MCC_ID","mcc.q3c","catchment_ID","lactoscan")],by.x="q5",by.y="MCC_ID")

names(farmers)[names(farmers) == 'q5'] <- 'MCC_ID'
## make a map with catchment ID coloring and pictures (not public)
pal <- colorFactor(
  palette = 'Dark2',
  domain = farmers$MCC_ID
)

m <- leaflet() %>% setView(lat = -0.6072, lng = 30.654, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=farmers, lng=~as.numeric(check.check2.Dairy._GPS_longitude), lat=~as.numeric(check.check2.Dairy._GPS_latitude),radius= 8, color=~pal(MCC_ID), popup = ~as.character(farmer_ID), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 



pal <- colorFactor(c("red", "green"),farmers$check.check2.Dairy.video_shown)
m <- leaflet() %>% setView(lat = -0.6072, lng = 30.654, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=farmers, lng=~as.numeric(check.check2.Dairy._GPS_longitude), lat=~as.numeric(check.check2.Dairy._GPS_latitude),radius= 1, color=~pal(check.check2.Dairy.video_shown), popup = ~as.character(check.check2.Dairy.video_shown), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

pal <- colorFactor(c("red", "green"),farmers$lactoscan)
m <- leaflet() %>% setView(lat = -0.6072, lng = 30.654, zoom=11)  %>%  addTiles(group="OSM") %>% addTiles(urlTemplate = "https://mts1.google.com/vt/lyrs=s&hl=en&src=app&x={x}&y={y}&z={z}&s=G",  group="Google", attribution = 'Google')  %>% addProviderTiles(providers$OpenTopoMap, group="Topography") %>% addCircleMarkers(data=farmers, lng=~as.numeric(check.check2.Dairy._GPS_longitude), lat=~as.numeric(check.check2.Dairy._GPS_latitude),radius= 1, color=~pal(lactoscan), popup = ~as.character(lactoscan), group = "X_uuid")   %>%  addLayersControl(baseGroups=c('OSM','Google','Topography')) 

###one ctrl MCC (MCC_50 - Ibaare) was replaced during farmer level reinforcement with Nicos, but at endline we found Ibaare open again so we interviewed both but kept Ibaare
###two treatmetn MCCs (MCC_356,MCC_225)  were replaced during treatment rollout
##MCC_290 was interviewed at baseline but closed at the time of milk analyzer distribution
###these people now all go to MCC_356
farmers$catchment_ID[farmers$MCC_ID == "MCC_290"] <- 66
farmers$lactoscan[farmers$MCC_ID == "MCC_290"] <- "T"
farmers$MCC_ID[farmers$MCC_ID == "MCC_290"] <- "MCC_356"

### export for Charles
#charles_set <- subset(farmers,check.check2.Dairy.video_shown == TRUE)
charles_set <- farmers
charles_set <- charles_set[c("enumerator","q3","q4","mcc.q3c","MCC_ID","mcc_name", "farmer_ID", "parish","village", "check.check2.Dairy.q10","check.check2.Dairy.q11", "check.check2.Dairy.q12", "check.check2.Dairy.q17", "check.check2.Dairy._GPS_latitude", "check.check2.Dairy._GPS_longitude","catchment_ID","lactoscan","check.check2.Dairy.video_shown")] 
names(charles_set) <- c("enumerator","mcc_district","mcc_sub","mcc_village","MCC_ID", "mcc_name","farmer_ID", "parish","village", "name","tel_1", "tel_2","name_head", "latitude", "longitude","catchment_ID","lacoscan", "video")
###remove trailing and leading spaces for names
charles_set$name <- trimws(charles_set$name)

#merge in MCC names from MCC sampling list


to_exp_farmers <- farmers[c("farmer_ID","q3","q4","parish","village","check.check2.Dairy._GPS_latitude","check.check2.Dairy._GPS_longitude")]

names(to_exp_farmers) <- c("farmer_ID","district", "subcounty", "parish","village" ,"latitude","longitude")

write.csv(to_exp_farmers,"farmer_ID_base_RFM.csv", row.names = FALSE) 

  
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





#replaceID <- read.csv("MCC_50_replace.csv")
#replaceID$MCC_ID <- "MCC_50"
#MCC[MCC$MCC_ID=="MCC_50",names(replaceID)] <- replaceID[1,]

replaceID <- read.csv("MCCS_replace.csv")
#replaceID$MCC_ID[1] <- "MCC_50"

#MCC[MCC$MCC_ID=="MCC_50",names(replaceID)] <- replaceID[1,]
MCC[MCC$MCC_ID=="MCC_356",names(replaceID)] <- replaceID[2,]
MCC[MCC$MCC_ID=="MCC_225",names(replaceID)] <- replaceID[3,]
MCC[MCC$MCC_ID=="MCC_363",names(replaceID)] <- replaceID[4,]

#remove MCC_290
MCC <- subset(MCC,MCC_ID!="MCC_290")

### export for wilberfoce

wilber_set <- MCC[,c("enumerator","district","sub",  "MCC_ID", "mcc.q1","mcc.q2","mcc.q3","mcc.q3a","mcc.q3b","mcc.q3c", "lactoscan")]
wilber_set <- subset(wilber_set, lactoscan=="T")
write.csv(wilber_set,file="list_wilber.csv", row.names=FALSE)

wilber_set <- MCC[,c("enumerator","district","sub",  "MCC_ID", "mcc.q1","mcc.q2","mcc.q3","mcc.q3a","mcc.q3b","mcc.q3c", "lactoscan","mcc._gps_latitude","mcc._gps_longitude")]
#wilber_set <- subset(wilber_set, lactoscan=="T")
write.csv(wilber_set,file="list_wilber_gps.csv", row.names=FALSE)

charles_set <- merge(charles_set,wilber_set, by.x="MCC_ID", by.y="MCC_ID")

names(charles_set)[names(charles_set) == 'mcc.q1'] <- 'MCC_name'
charles_set$district <- trimws(charles_set$district)
charles_set$sub <- trimws(charles_set$sub)
charles_set$MCC_name <- trimws(charles_set$MCC_name)
charles_set$name <- trimws(charles_set$name)
charles_set$name_head <- trimws(charles_set$name_head)

charles_set$name <- ifelse( charles_set$name_head == "n/a", charles_set$name, paste(paste(charles_set$name, "for head:", sep = " "), charles_set$name_head, sep=" ")) 


mcc_set <- distinct(charles_set[c("MCC_ID","district","sub","MCC_name","mcc.q2", "mcc.q3","mcc.q3a", "mcc.q3b","mcc.q3c", "lactoscan", "mcc._gps_latitude", "mcc._gps_longitude","catchment_ID")])


charles_set <-charles_set[c("district","sub","MCC_ID","MCC_name","parish",	"village",	"name", "farmer_ID", "tel_1",	"tel_2", "latitude",	"longitude",	"catchment_ID",	"lactoscan",	"video"
)]



charles_set <- charles_set[with(charles_set, order(charles_set[,1], charles_set[,2], charles_set[,3])), ]


charles_set$name[duplicated(charles_set$name)]

write.csv(charles_set,file="list_endline.csv", row.names=FALSE)

mcc_set <- mcc_set[c("district",	"sub","catchment_ID","MCC_name",	 "mcc.q3b",	"mcc.q3c",  "MCC_ID","mcc.q2",	"mcc.q3",	"mcc.q3a","mcc._gps_latitude",	"mcc._gps_longitude",	"lactoscan")]

          
names(mcc_set) <- c("district",	"sub", "catchment_ID", "name","parish",	"village", "MCC_ID","manager",	"tel_1",	"tel_2"	,"latitude",	"longitude",		"lactoscan")          
mcc_set <- mcc_set[with(mcc_set, order(mcc_set[,1], mcc_set[,2], mcc_set[,3], mcc_set[,4])), ]

mcc_set$name <- trimws(mcc_set$name)



write.csv(mcc_set,file="list_endline_MCC.csv", row.names=FALSE)
 
to_exp <- MCC[c("MCC_ID", "district", "sub","mcc.q3b","mcc.q3c" ,"mcc._gps_latitude","mcc._gps_longitude")]
names(to_exp) <- c("MCC_ID","district", "subcounty", "parish","village" ,"latitude","longitude")

write.csv(to_exp,"MCC_ID_base_RFM.csv", row.names = FALSE) 

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

names(farmers) <- sub("check.check2.Dairy.", "",names(farmers))
names(MCC) <- sub("mcc.", "",names(MCC))
names(MCC) <- sub("assets.", "",names(MCC))
names(MCC) <- sub("day.", "",names(MCC))
names(MCC) <- sub("services.", "",names(MCC))
write.csv(farmers,"/home/bjvca/data/projects/OneCG/RMVC/baseline/data/public/farmers.csv", row.names=FALSE)
write.csv(MCC,"/home/bjvca/data/projects/OneCG/RMVC/baseline/data/public/MCCs.csv", row.names=FALSE)