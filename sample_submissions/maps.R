library(sf)
library(sp)
library(rmapshaper)
library(stringr)
library(lubridate)
library(leaflet)

## read in shape file for parishes of Uganda
data.p <- sf::st_read("shapefiles/PARISHES_2016_UTM_36N.shp") %>% 
  st_transform(4326) %>%
  rmapshaper::ms_simplify()
###fix issues with polygons
data.p  <- st_make_valid(data.p )

##center map on mbarara
lng.center <- 30.65
lat.center <- -0.60
zoom.def <- 9

#get  MCC info (including gps coordinates)
mccs <- read.csv("list_wilber.csv")

#find indices in shapefile for MCCs gps locations points that fall in the parish polygons
index_coord <- array(NA, dim(mccs)[1])
for (i in 1:dim(mccs)[1]) {
test_coord <- data.frame(lat =mccs$mcc._gps_latitude[i]  , lon = mccs$mcc._gps_longitude[i])
test_point <- st_as_sf(test_coord, coords = c("lon", "lat"), crs = 4326)
index_coord[i] <- st_within(test_point, data.p)
}

index_coord <- unlist(index_coord)

mccs <- cbind(mccs,data.p[index_coord,])

### get price means from submission data 
dta <- read.csv("dta_reports.csv")
#### choose parameter to plot

parameter <- "Price"
####

###choose dates (last week)

end <- Sys.Date()
start <- end - 14
dta <- dta[dta$date>start & dta$date<=end,] 

### aggregate
dta <- data.frame(tapply(dta[,parameter],dta["MCC_ID"], FUN=mean, na.rm=T))

dta$MCC_ID <- rownames(dta)
names(dta) <- c("price","MCC_ID")
### merge price means to mcc data (that now also has associated polygons)
mccs <- merge(mccs, dta, by="MCC_ID")
mccs <- sf::st_as_sf(mccs)
 
### plot the price means
leaflet(data = mccs)  %>% 
  addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>% 
  setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
  addPolygons(group = 'price', 
              fillColor = ~colorNumeric(c("#008000","#FF0000"), domain =  mccs$price , reverse = TRUE)(price), 
              fillOpacity = 0.9,
              color = 'black',  label = ~as.character(price), popup = ~as.character(round(price)) ,
              weight = 1.5)  %>%
  addLegend(pal = colorNumeric(c("#008000","#FF0000"), domain =  mccs$price , reverse = TRUE), values = mccs$price, opacity = 0.7, title = NULL,
            position = "topright") 