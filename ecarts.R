
library(sf)
library(leaflet)
library(lubridate)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(htmlwidgets)
## importer le GPX avec sf:st_read, rgdal::readOGR ou plotKML::readGPX 
# perd les datetime  des points. J'ai oublié où j'ai lu ça, mais apparemment 
# c'est normal car les shapefile ont juste la date.  Faisons le quand même.
way <- st_read("ecarts.gpx", layer = "waypoints") %>% st_transform(., "+proj=longlat +datum=WGS84") 
tracks <- st_read("ecarts.gpx", layer = "tracks") %>% st_transform(., "+proj=longlat +datum=WGS84") 



### les kml il faut enlever l'axe des Z avec st_zm, sinon on a une erreur opaque
# https://github.com/r-spatial/mapview/issues/98
# Error in if (length(nms) != n || any(nms == "")) stop("'options' must be a fully named list, or have no names (NULL)") : 
#missing value where TRUE/FALSE needed
couture <- st_read("la-trail-des-couture.kml") %>% st_zm() %>% rename(name = Name) %>% select(name, geometry)
crete <- st_read("la-crete-en-tete.kml")%>% st_zm()%>% rename(name = Name) %>% select(name, geometry)

## une alternative est de parser le GPX du garmin, ce qui va garder les datestamp:
#https://shiring.github.io/maps/2017/04/09/gran_canaria
# Parse the GPX file

library(XML)
pfile <- htmlTreeParse("ecarts.gpx", useInternalNodes = T)
elevations <- as.numeric(as.character(xpathSApply(pfile, path = "//trkpt/ele", xmlValue)))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
# Extract latitude and longitude from the coordinates
lats <- as.numeric(as.character(coords["lat",]))
lons <- as.numeric(as.character(coords["lon",]))
# Put everything in a sf data.frame
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
geodata <- geodf
geodata$time <- as.character(strptime(geodata$time, format = "%Y-%m-%dT%H:%M:%SZ"))
z <- st_as_sf(geodata,coords = c("lon","lat")) %>%
  mutate(mytime = as_datetime(time),
         mytime2 = as.numeric(mytime),
         rownum = row_number()) %>% 
  st_set_crs(4326) %>% st_transform(., "+proj=longlat +datum=WGS84") 


#### DÉBUT : EXPLORATION
#### Map the tracks
# mypal <- colorNumeric(palette = viridis(5), 
#                       domain = z$rownum)

## 4649 points
# z %>%   leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name, " - ", time)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")
# 
# #grand-ecart
# z %>%  slice(1:1492)  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")
# #marco
# z%>%  slice(1494:1942)  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")
# 
# #chevreuil
# z%>%  slice(1943:2120)  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")
# #eric
# z%>%  slice(2121:2452)  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")
# 
# #chesterfield
# z%>%  slice(2476:2708)  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")

# #les points utiles
# z%>%  slice( c(1:1013, 
#                1255:2500, 
#                2571:2645, 
#                2896:2920, 
#                3695:3720, 
#                4376:4396,
#                4435:4459))  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")
# les points de la 2eme loop, j utilise des échantillons pour compléter ce qui manque
# dans la premiere loop
# z%>%  slice(2709:4649)  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")
#### FIN : EXPLORATION

#### DÉBUT : BRICOLAGE DES SEGMENTS 
# grand ecart
ge_ligne <- rbind( way%>% filter (name == "GE") %>% select(geometry),
                     z %>% select(geometry) %>% slice(c (
                       1492:1492,
                       1:521, 
                       3695:3720,
                       716:1013,
                       1255:1492))) %>%    
  mutate(name = "Grand Écart")

# grand ecart extra
gee_ligne <- z %>% select(geometry) %>% slice(c (
                     521:716))  %>% 
  mutate(name = "Grand Écart (extra)")

# marco
marco_ligne <- z %>% select(geometry) %>% slice(c (
  2920:2920,
  1494:1942))  %>% 
  mutate(name = "Lacets de Marco")

#chevreuil
chev_ligne <- z %>% select(geometry) %>% slice(c (
  1942:2120)) %>% 
  mutate(name = "Chevreuil")

#eric
eric_ligne <- z %>% select(geometry) %>% slice(c (
  2120:2475))  %>% 
  mutate(name = "La Flamme à Éric")

#chesterfield1
chesterfield_ligne1 <- 
  rbind(
    z %>% select(geometry) %>% slice(2500:2475) ,
    z %>% select(geometry) %>% slice(2571:2645),
    z %>% select(geometry) %>% slice(1450:1450)
    )  %>% 
  mutate(name = "Chesterfield 1")

# chesterfield2
chesterfield_ligne2 <- 
  rbind(
    z %>% select(geometry) %>% slice(1457:1457),
    z %>% select(geometry) %>% slice(4396:4376),
    z %>% select(geometry) %>% slice(2920:2920),
    way%>% filter (name == "GE") %>% select(geometry)) %>% 
  mutate(name = "Chesterfield 2")

#### FIN : BRICOLAGE DES SEGMENTS 

#### DÉBUT: Convertir les points en lignes, greffer couture et crete, calculer la longueur
all <- rbind( ge_ligne,
              gee_ligne,
              marco_ligne,
              eric_ligne,
              chev_ligne,
              chesterfield_ligne1,
              chesterfield_ligne2
              ) %>%
  group_by(name) %>%
  summarize(., do_union = FALSE) %>%
  st_cast("LINESTRING")  %>%
  rbind(couture, crete) %>%
  mutate(longueur = st_length(.),
         name_legende = paste0(name, " - ", round(longueur), " m")) 
#### FIN: Convertir les points en lignes

#### DÉBUT : Créer le leaflet

#mypal <- leaflet::colorFactor(viridis_pal(option="C")(7), domain = all$name_legende)
mypal <- leaflet::colorFactor(brewer.pal(n=9, name= "Dark2"), 
                              domain = all$name_legende)

all %>% leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolylines(color = ~ mypal(name_legende)) %>%
  addMarkers(data= way %>% filter(name == "GE"), label = ~ paste0("Départ"))  %>%
  addLegend("bottomleft",
            pal = mypal,
            values = ~ name_legende,
            title = "Sentiers de la section des écarts") 
  


mymap <-  all %>% leaflet() %>% 
    addProviderTiles(providers$Esri.WorldTopoMap, group= "topo") %>%
    addProviderTiles(providers$Esri.WorldImagery, group= "satellite") %>%
    addPolylines(color = ~ mypal(name_legende)) %>%
    addMarkers(data= way %>% filter(name == "GE"), 
               label = ~ paste0("Départ"),
               group = "Afficher départ")  %>%
  addLegend("bottomleft",
            pal = mypal,
            values = ~ name_legende,
            title = "Sentiers de vélo de la section des écarts")  %>%
  addLayersControl(
    baseGroups = c("satellite", "topo"),
    overlayGroups = c("Afficher départ"),
    options = layersControlOptions(collapsed = FALSE))  
  
mymap
saveWidget(mymap, file = "carte_ecarts.html", selfcontained = T)
#### FIN : Créer le leaflet


couture %>% st_zm %>%  select(-Description) %>% leaflet() %>% addPolylines()

