
library(sf)
library(leaflet)
library(lubridate)
library(dplyr)
library(viridis)
## import avec st_read, rgdal::readOGR et plotKML::readGPX perdent les datetime  des points.
# J'ai oublié où, mais apparemment c'est normal, j'ai lu quelque part que les shapefile ont juste la date.
way <- st_read("ecarts.gpx", layer = "waypoints") %>% st_transform(., "+proj=longlat +datum=WGS84") 
tracks <- st_read("ecarts.gpx", layer = "tracks") %>% st_transform(., "+proj=longlat +datum=WGS84") 

tracks  %>%  leaflet()%>%   
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addPolylines() 

way %>%   leaflet()%>%   
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircles(label = ~ paste0(name)) 

## une alternative est de parser le GPX du garmin:
#https://shiring.github.io/maps/2017/04/09/gran_canaria
# Parse the GPX file
library(XML)
pfile <- htmlTreeParse("ecarts.GPX", useInternalNodes = T)
elevations <- as.numeric(as.character(xpathSApply(pfile, path = "//trkpt/ele", xmlValue)))
times <- xpathSApply(pfile, path = "//trkpt/time", xmlValue)
coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
# Extract latitude and longitude from the coordinates
lats <- as.numeric(as.character(coords["lat",]))
lons <- as.numeric(as.character(coords["lon",]))
# Put everything in a dataframe
geodf <- data.frame(lat = lats, lon = lons, ele = elevations, time = times)
geodata <- geodf
geodata$time <- as.character(strptime(geodata$time, format = "%Y-%m-%dT%H:%M:%SZ"))

 
z <- st_as_sf(geodata,coords = c("lon","lat")) %>%
  mutate(mytime = as_datetime(time),
         mytime2 = as.numeric(mytime),
         rownum = row_number()) %>% 
  st_set_crs(4326) %>% st_transform(., "+proj=longlat +datum=WGS84") 


mypal <- colorNumeric(palette = viridis(5), 
                      domain = z$rownum)

## 4649 points
z %>%   leaflet()%>%   
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
             label =~ paste0(rownum)) %>%
  addMarkers(data= way, label = ~ paste0(name, " - ", time)) %>%
  addLegend("bottomleft",
            pal = mypal,
            values = ~ rownum,
            title = "rownum")

#st_crs(z)

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

#les points utiles
z%>%  slice( c(1:1013, 
               1255:2500, 
               2571:2645, 
               2896:2920, 
               3695:3720, 
               4376:4396,
               4435:4459))  %>%leaflet()%>%   
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
             label =~ paste0(rownum)) %>%
  addMarkers(data= way, label = ~ paste0(name)) %>%
  addLegend("bottomleft",
            pal = mypal,
            values = ~ rownum,
            title = "rownum")




# la 2eme loop, j utilise des échantillons pour compléter la premiere loop

# z%>%  slice(2709:4649)  %>%leaflet()%>%   
#   addProviderTiles(providers$Esri.WorldTopoMap) %>%
#   addCircles(color= ~ mypal(rownum), fillOpacity=0.4, radius =0.1,
#              label =~ paste0(rownum)) %>%
#   addMarkers(data= way, label = ~ paste0(name)) %>%
#   addLegend("bottomleft",
#             pal = mypal,
#             values = ~ rownum,
#             title = "rownum")

### ok créons des segments pour faire les lignse

# grand ecart
ge_ligne <- rbind( way%>% filter (name == "GE") %>% select(geometry),
                     z %>% select(geometry) %>% slice(c (
                       1492:1492,
                       1:521, 
                       3695:3720,
                       716:1013,
                       1255:1492))) # %>% st_coordinates() %>% st_linestring()
#ge_ligne <- do.call(c, st_geometry(ge_ligne)) %>% st_cast("LINESTRING")
#%>%    mutate(name = "GE")

## sollution 1 st_linestring crée une saloperie qui n'est pas un df.frame.. mais au moins ça garde l'ordre des points
## solution 2 st_cast crée un sf data frame, mais pas dans le bon ordre..
# grand ecart extra
gee_ligne <- z %>% select(geometry) %>% slice(c (
                     522:715))  %>% 
  mutate(name = "GE extra")
gee_ligne <- do.call(c, st_geometry(gee_ligne)) %>% st_cast("LINESTRING")
# marco
marco_ligne <- z %>% select(geometry) %>% slice(c (
  2920:2920,
  1494:1942))  %>% 
  mutate(name = "Lacets de Marco")
marco_ligne <- do.call(c, st_geometry(marco_ligne)) %>% st_cast("LINESTRING")
#chevreuil
chev_ligne <- z %>% select(geometry) %>% slice(c (
  1942:2120)) %>% 
  mutate(name = "Chevreuil")
chev_ligne <- do.call(c, st_geometry(chev_ligne)) %>% st_cast("LINESTRING")
#eric
eric_ligne <- z %>% select(geometry) %>% slice(c (
  2120:2452))  %>% 
  mutate(name = "La Flamme à Éric")
eric_ligne <- do.call(c, st_geometry(eric_ligne)) %>% st_cast("LINESTRING")
#chesterfield1
chesterfield_ligne1 <- 
  rbind(
    z %>% select(geometry) %>% slice(2500:2475) ,
    z %>% select(geometry) %>% slice(2571:2645))  %>% 
  mutate(name = "Chesterfield")
chesterfield_ligne1 <- do.call(c, st_geometry(chesterfield_ligne1)) %>% st_cast("LINESTRING")
# chesterfield2
chesterfield_ligne2 <- 
  rbind(
    z %>% select(geometry) %>% slice(4396:4376),
    z %>% select(geometry) %>% slice(2920:2920),
    way%>% filter (name == "GE") %>% select(geometry)) %>% 
  mutate(name = "Chesterfield")
chesterfield_ligne2 <- do.call(c, st_geometry(chesterfield_ligne2)) %>% st_cast("LINESTRING")

all <- rbind( ge_ligne,
              gee_ligne,
              marco_ligne,
              eric_ligne,
              chev_ligne,
              chesterfield_ligne1,
              chesterfield_ligne2
              ) %>%
  mutate(name = as.factor(name), rownum= row_number())

mypal <- leaflet::colorFactor(viridis_pal(option="C")(7), domain = all$name)

all %>% leaflet() %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addCircles(color = ~ mypal(name),
             radius= 0.1)

zz<- all %>% group_by(name) %>% summarise(m = mean(rownum)) %>% st_cast("LINESTRING")
do.call(c,st_geometry(zz))
plot(zz)
chesterfield_ligne2 %>%  leaflet()%>%   
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolylines() %>%
  addMarkers(data= way, label = ~ paste0(name))

sol2 <- pts_sf %>% group_by(id) %>% summarize(m = mean(attr_data)) %>% st_cast("LINESTRING")
sol2ches<- chesterfield_ligne2 %>% mutate(name = "ches", rownum = row_number()) %>%
  group_by(name) %>% summarize(m = mean(rownum)) %>% st_cast("LINESTRING")
plot(sol2ches)

sol2chess<- chesterfield_ligne2 %>% mutate(name = "ches") %>%
  group_by(name) %>% summarize() %>% st_cast("LINESTRING")
plot(sol2chess)

sol2ge <- ge_ligne %>% mutate(name = "ches", rownum = row_number()) %>%
  group_by(name) %>% summarize(m = mean(rownum)) %>% st_cast("LINESTRING")
plot(sol2ge)
#https://github.com/r-spatial/sf/issues/321
#creating lines from points w/ grouping


pts_sf <- data.frame(
  x = seq(47, 48, by=0.1),
  y = seq(147, 148, by=0.1),
  attr_data = rnorm(11,42,42),
  id = c(rep("fred",6), rep("wilma",5))
) %>% 
  sf::st_as_sf(coords = c("x","y")) %>% 
  sf::st_set_crs(4326)

plot(pts_sf)
sol1 <- pts_sf%>% st_coordinates() %>% st_linestring()
sol2 <- pts_sf %>% group_by(id) %>% summarize(m = mean(attr_data)) %>% st_cast("LINESTRING")
plot(sol2)
sol3 <- z %>% mutate(blank=1) %>% select(blank) %>% group_by(blank) %>%   st_cast("MULTILINESTRING")

head(pts_sf) ; class(pts_sf)
head(z); class(z)
sol3 <- z %>% mutate(name = pouet, blank=1) %>% select(name, blank) %>% group_by(name) %>%    summarize(blank = mean(blank)) %>%  st_cast("LINESTRING")

plot(sol3)
plot(z)
plot(sol2)
sol4 <- z %>%    summarise(blank= 1) %>%  st_cast("MULTILINESTRING")
plot(sol4)


sol3 %>%   leaflet()%>%   
  addProviderTiles(providers$Esri.WorldTopoMap) %>%
  addPolylines()
