library(sf)
library(sp)
library(rmapshaper)
library(stringr)
library(lubridate)
library(leaflet)

## read in shape file for parishes of Uganda
data.p <- sf::st_read("PARISHES_2016_UTM_36N.shp") %>% 
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
dta <- read.csv("https://raw.githubusercontent.com/bjvca/RMVC/master/sample_submissions/dta_reports.csv")
#### choose parameter to plot
parameter <- "Price"
today <- Sys.Date()
end <- today - 14
dta_plot <- dta[dta$date>end  & dta$date<=today,] 

### aggregate
dta_plot <- data.frame(tapply(dta_plot[,parameter],dta_plot["MCC_ID"], FUN=mean, na.rm=T))

dta_plot$MCC_ID <- rownames(dta_plot)
names(dta_plot) <- c("Sel_par","MCC_ID")
### merge price means to mcc data (that now also has associated polygons)
mccs_plot <- merge(mccs, dta_plot, by="MCC_ID")
mccs_plot <- sf::st_as_sf(mccs_plot)
 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("MCC Performance"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "variable", label = "Choose a statistic", choices = c("Fat","SNF","Protein","Added.Water","Price")),
      dateRangeInput(inputId = "dateRange", label = "Select Date Range", 
                     start = "2023-10-14", end = Sys.Date())
    ),
    # Output of the map
    mainPanel(
      leafletOutput("maps",width=600, height=1200)
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  output$maps <- renderLeaflet({
  ### plot the price means
  leaflet(data = mccs_plot)  %>% 
    addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 1), group = "Open Street Map") %>% 
    setView(lng = lng.center, lat = lat.center, zoom = zoom.def) %>%
    addPolygons(group = 'Sel_par', 
                fillColor = ~colorNumeric(c("#008000","#FF0000"), domain =  mccs_plot$Sel_par , reverse = TRUE)(Sel_par), 
                fillOpacity = 0.9,
                color = 'black',  label = ~as.character(Sel_par), popup = ~as.character(round(Sel_par)) ,
                weight = 1.5)  %>%
    addLegend(pal = colorNumeric(c("#008000","#FF0000"), domain =  mccs_plot$Sel_par , reverse = TRUE), values = mccs_plot$Sel_par, opacity = 0.7, title = NULL,
              position = "topright") 
  })
  
  get_data_p <- reactive({

  
    dta_plot <- dta[dta$date>input$dateRange[1]  & dta$date<=input$dateRange[2],]

    ### aggregate
    dta_plot <- data.frame(tapply(dta_plot[,input$variable],dta_plot["MCC_ID"], FUN=mean, na.rm=T))

    dta_plot$MCC_ID <- rownames(dta_plot)
    names(dta_plot) <- c("Sel_par","MCC_ID")
    ### merge price means to mcc data (that now also has associated polygons)
    mccs_plot <- merge(mccs, dta_plot, by="MCC_ID")
   sf::st_as_sf(mccs_plot)



  })
  
  observe({
    data <- get_data_p()
    leafletProxy('maps', data = data) %>%
      clearGroup('Sel_par') %>%
      clearControls() %>%
  addPolygons(group = 'Sel_par', 
              fillColor = ~colorNumeric(c("#008000","#FF0000"), domain =  mccs_plot$Sel_par , reverse = TRUE)(Sel_par), 
              fillOpacity = 0.9,
              color = 'black',  label = ~as.character(Sel_par), popup = ~as.character(round(Sel_par)) ,
              weight = 1.5)  %>%
    addLegend(pal = colorNumeric(c("#008000","#FF0000"), domain =  mccs_plot$Sel_par , reverse = TRUE), values = mccs_plot$Sel_par, opacity = 0.7, title = NULL,
              position = "topright") 
  })


  
 
}

# Run the application 
shinyApp(ui = ui, server = server)
