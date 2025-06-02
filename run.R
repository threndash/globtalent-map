library(leaflet)
library(htmltools)
library(htmlwidgets)
library(rnaturalearth)
library(sf)
library(data.table)
library(rmapshaper)

<<<<<<< HEAD
=======
setwd("~/Library/CloudStorage/GoogleDrive-hrendash@globtalent.org/My Drive/Globtalent Web/globtalent-map")

>>>>>>> 2d88f61daedc3b27506a66caad7ddc17ad14f54c
rotatedMarker <- htmlDependency(
  "Leaflet.rotatedMarker",
  "0.1.2",
  src = normalizePath("."),
  script = "leaflet.rotatedMarker.js"
)

registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}

<<<<<<< HEAD
file <- "https://raw.githubusercontent.com/threndash/logo/master/"
=======
pins_path <- "https://raw.githubusercontent.com/threndash/globtalent-map/main/pins/"
>>>>>>> 2d88f61daedc3b27506a66caad7ddc17ad14f54c

world <- ne_countries(scale = "medium", returnclass = "sf")
world_large <- ne_countries(scale = "large", returnclass = "sf")

ocean_polygon <- st_polygon(list(rbind(
  c(-180, -90), c(180, -90), c(180, 90), c(-180, 90), c(-180, -90)
))) %>% 
  st_sfc(crs = 4326) %>% 
  st_sf(data.frame(id = 1, geometry = .))

world_centroids <- st_centroid(world)
centroid_coords <- st_coordinates(world_centroids)

centroids_df <- data.frame(
  admin = world$admin,
  longitude = centroid_coords[,1],
  latitude = centroid_coords[,2]
)

<<<<<<< HEAD
dt <- readxl::read_excel("~/Library/CloudStorage/GoogleDrive-hrendash@globtalent.org/My Drive/Globtalent Web/globtalent-map/input_data.xlsx")
=======
dt <- readxl::read_excel("input_data.xlsx")
>>>>>>> 2d88f61daedc3b27506a66caad7ddc17ad14f54c
names(dt) <- paste0("country.",gsub("[^A-Z]","",names(dt)))
dt <- reshape(as.data.frame(dt),v.names = "country",varying = grep("^country",names(dt),value = T),timevar = "program",
              times = gsub("country\\.","",grep("^country",names(dt),value = T)),direction = "long")
dt <- dt[!is.na(dt$country),]

dt$country[dt$country=="Bosnia"] <- "Bosnia and Herzegovina"
dt$country[dt$country=="Cameroun"] <- "Cameroon"
dt$country[dt$country=="Côte d'Ivoire"] <- "Ivory Coast"
dt$country[dt$country=="Czech Republic"] <- "Czechia"
dt$country[dt$country=="DRC"] <- "Democratic Republic of the Congo"
dt$country[dt$country=="Eswatini"] <- "eSwatini"
dt$country[dt$country=="Macedonia"] <- "North Macedonia"
dt$country[dt$country=="Salvador"] <- "El Salvador"
dt$country[dt$country=="Serbia"] <- "Republic of Serbia"
dt$country[dt$country=="Tanzania"] <- "United Republic of Tanzania"

dt <- unique(dt)
dt <- data.table(dt)
dt <- dt[,id := NULL]
dt <- dt[,n_programs:=length(unique(program)),by="country"]
dt <- dt[,all_programs:=paste(sort(unique(program)),collapse = ", "),by="country"]
dt <- dt[order(dt$country,dt$program),]
dt <- dt[,ord_program:=seq_len(length(unique(program))),by="country"]

dt$ang <- 0
dt$ang[dt$n_programs==2 & dt$ord_program==1] <- 320
dt$ang[dt$n_programs==2 & dt$ord_program==2] <- 40

dt$ang[dt$n_programs==3 & dt$ord_program==1] <- 300
dt$ang[dt$n_programs==3 & dt$ord_program==2] <- 0
dt$ang[dt$n_programs==3 & dt$ord_program==3] <- 60

dt$ang[dt$n_programs==4 & dt$ord_program==1] <- 0
dt$ang[dt$n_programs==4 & dt$ord_program==2] <- 90
dt$ang[dt$n_programs==4 & dt$ord_program==3] <- 180
dt$ang[dt$n_programs==4 & dt$ord_program==4] <- 270

<<<<<<< HEAD
dt$file <- paste0(file,tolower(dt$program),".svg")
=======
dt$file <- paste0(pins_path,tolower(dt$program),".svg")
>>>>>>> 2d88f61daedc3b27506a66caad7ddc17ad14f54c

setdiff(dt$country,centroids_df$admin)

dt <- merge(dt,centroids_df,by.x="country",by.y="admin")

selected_countries_data <- world_large[world_large$admin %in% dt$country, ]
# selected_countries_highres <- ms_simplify(selected_countries_data, keep = 0.99, keep_shapes = TRUE)
cols <- c("country", "all_programs")
selected_countries_data <- merge(selected_countries_data,unique(dt[,..cols]),by.x="admin",by.y="country")

leafIcons <- icons(
  dt$file,
  iconWidth = 10*0.75, iconHeight = 10
)

<<<<<<< HEAD
mytext <- paste(
  "Country: ", selected_countries_data$admin,"<br/>",
=======
selected_countries_data$admin_label <- selected_countries_data$admin
selected_countries_data$admin_label[selected_countries_data$admin_label=="Palestine"] <- "West Bank and Gaza"

dt$country_label <- dt$country
dt$country_label[dt$country_label=="Palestine"] <- "West Bank and Gaza"

mytext <- paste(
  "Country: ", selected_countries_data$admin_label,"<br/>",
>>>>>>> 2d88f61daedc3b27506a66caad7ddc17ad14f54c
  "Programs: ", selected_countries_data$all_programs,
  sep="") %>%
  lapply(htmltools::HTML)

mytext_markers <- paste(
<<<<<<< HEAD
  "Country: ", dt$country,"<br/>",
=======
  "Country: ", dt$country_label,"<br/>",
>>>>>>> 2d88f61daedc3b27506a66caad7ddc17ad14f54c
  "Programs: ", dt$all_programs,
  sep="") %>%
  lapply(htmltools::HTML)

<<<<<<< HEAD
legend_html <- "
<div style='display: flex; align-items: center;'>
    <img src='https://raw.githubusercontent.com/threndash/logo/master/star.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>STAR</span>
</div>
<div style='display: flex; align-items: center; margin-top: 5px;'>
    <img src='https://raw.githubusercontent.com/threndash/logo/master/nations.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>NATIONS</span>
</div>
<div style='display: flex; align-items: center; margin-top: 5px;'>
    <img src='https://raw.githubusercontent.com/threndash/logo/master/big.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>BIG</span>
</div>
<div style='display: flex; align-items: center; margin-top: 5px;'>
    <img src='https://raw.githubusercontent.com/threndash/logo/master/excl.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>EXCL</span>
</div>
"

lf <- leaflet( data = dt, options = leafletOptions(scrollWheelZoom = FALSE) ) %>%
  # addProviderTiles( providers$Thunderforest.OpenCycleMap ) %>%
  setView( lat=20, lng=20 , zoom=3) %>%
  registerPlugin( plugin = rotatedMarker ) %>%
  addPolygons(
    data = ocean_polygon, 
    fillColor = "#8AA99F", # Ocean color
    fillOpacity = 1,
    stroke = FALSE
  ) %>%
  addPolygons(data = world, color = "#404040", fillColor = "#ffffff", weight = 1, fillOpacity = 0.7) %>%
  addPolygons(data = selected_countries_data, color = "#404040", fillColor = "#cacaca",  weight = 1, fillOpacity = 1,
=======
legend_html <- paste0("
<div style='display: flex; align-items: center;'>
    <img src='",pins_path,"star.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>STAR</span>
</div>
<div style='display: flex; align-items: center; margin-top: 5px;'>
    <img src='",pins_path,"nations.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>NATIONS</span>
</div>
<div style='display: flex; align-items: center; margin-top: 5px;'>
    <img src='",pins_path,"big.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>BIG</span>
</div>
<div style='display: flex; align-items: center; margin-top: 5px;'>
    <img src='",pins_path,"excl.svg' width='10px' height='13.3px'>
    <span style='margin-left: 5px;'>EXCL</span>
</div>
")

lf <- leaflet( data = dt, options = leafletOptions(scrollWheelZoom = FALSE, zoomSnap = 0.1) ) %>%
  # addProviderTiles( providers$Thunderforest.OpenCycleMap ) %>%
  setView( lat=20, lng=20 , zoom=2.8) %>%
  registerPlugin( plugin = rotatedMarker ) %>%
  addPolygons(
    data = ocean_polygon, 
    fillColor = "#89BCBC", # Ocean color
    fillOpacity = 1,
    stroke = FALSE
  ) %>%
  addPolygons(data = world, color = "#404040", fillColor = "#f2f0e9", weight = 1, fillOpacity = 1) %>%
  addPolygons(data = selected_countries_data, color = "#404040", fillColor = "#CFCFCF",  weight = 1, fillOpacity = 1,
>>>>>>> 2d88f61daedc3b27506a66caad7ddc17ad14f54c
              label = mytext,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              )) %>%
  addMarkers( lng = ~longitude
              , lat = ~latitude
              , icon = leafIcons
              , options = markerOptions( rotationAngle = ~ang )
              , label = mytext_markers,
              labelOptions = labelOptions( 
                style = list("font-weight" = "normal", padding = "3px 8px"), 
                textsize = "13px", 
                direction = "auto"
              )) %>%
  addControl(html = legend_html, position = "topright")
  # addLegend( colors = c("#1f77b4", "#ff7f0e", "#2ca02c","#d62728"), 
  #            labels = c("STAR", "NATIONS", "BIG", "EXCL"),
  #            opacity=0.9, title = "Programs", position = "topright" )
saveWidget(lf, file = "index.html")
