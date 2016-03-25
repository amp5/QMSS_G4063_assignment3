setwd("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/assignment3")
load("geo_tagged_tweets.Rdata")

library(sp)
library(maps)
library(maptools)
library(plyr)
library(RColorBrewer)
library(RCurl)
library(bitops)
library(rjson)
library(ggplot2)
library(grid)
library(streamR)
gpclibPermit()

data <- geo_tagged_tweets

filteredData <- data[!(is.na(data$lat)) | !(is.na(data$place_lat)),]
save (filteredData, file= 'geo_filtered_data.Rdata')
View(filteredData)
class(filteredData) # dataframe

filteredData$text <- sapply(filteredData$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
#Clinton, Cruz, Rubio, Sanders, Trump
HC_all <- subset (filteredData, grepl(pattern =  "Clinton | clinton | Hillary | hillary | Hillaryclinton | hillaryclinton | Hillary Clinton | hillary clinton" , 
                                      filteredData$text, ignore.case = TRUE))
BS_all <- subset (filteredData, grepl(pattern =  "Berniesanders | berniesanders | Bernie Sanders  | bernie sanders | Bernie | bernie | Sensanders | sensanders" , 
                                      filteredData$text, ignore.case = TRUE))
TC_all <-  subset (filteredData, grepl(pattern =  "Cruz | cruz | Ted | ted | Tedcruz | tedcruz | Ted Cruz | ted cruz" , 
                                       filteredData$text, ignore.case = TRUE))
DT_all <- subset (filteredData, grepl(pattern =  "Donaldtrump  | donaldtrump | Donald Trump | donald trump | Trump | trump | Donald | donald | Trumpf | trumpf" , 
                                      filteredData$text, ignore.case = TRUE))
MR_all <- subset (filteredData, grepl(pattern =  "Marcorubio | marcorubio | Marco Rubio | marco rubio" , 
                                      filteredData$text, ignore.case = TRUE))

useful_info <- c("text", "id_str", "created_at", "screen_name", "place_lat", "place_lon",  "lat", "lon", "country_code")
HC <-  HC_all[useful_info]
BS <- BS_all[useful_info]
TC <- TC_all[useful_info]
DT <- DT_all[useful_info]
MR <- MR_all[useful_info]

save (HC, file= 'HC.Rdata')
save (BS, file= 'BS.Rdata')
save (TC, file= 'TC.Rdata')
save (DT, file= 'DT.Rdata')
save (MR, file= 'MR.Rdata')

library(ggplot2)
library(maps)

world <- map_data("world")
US_states <- map_data("state")
ggplot()+ geom_polygon( data=world, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
#Clinton
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
#Sanders
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
#Cruz
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
#Rubio
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
#Trump
ggplot(world) + geom_map(aes(map_id = region), map = world, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = world$long, y = world$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")

#class(HC)


#install.packages("rworldmap") 


library(rworldmap)
library(maps)

useful_info <- c("text", "id_str", "created_at", "screen_name", "place_lat", "place_lon",  "lat", "lon", "country_code")
HC_us <- HC[HC$country_code=='US',] 
BS_us <- BS[BS$country_code=='US',]
TC_us <- TC[TC$country_code=='US',]
DT_us <- DT[DT$country_code=='US',]
MR_us <- MR[MR$country_code=='US',]

ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")

#############################
# Part 2 - Counting by State
#############################



# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}


geo_pts <- c("place_lon", "place_lat")
HC_pt <-  HC_us[geo_pts]
BS_pt <- BS_us[geo_pts]
TC_pt <- TC_us[geo_pts]
DT_pt <- DT_us[geo_pts]
MR_pt <- MR_us[geo_pts]

all_pt <- rbind(HC_pt, BS_pt, TC_pt, DT_pt, MR_pt)

#ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_pt, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")


HC_pt$state <- latlong2state(HC_pt)
filteredHC <- HC_pt[!(is.na(HC_pt$state)),]
count(filteredHC, "state")
state_HC <- count(filteredHC, "state")

BS_pt$state <- latlong2state(BS_pt)
filteredBS <- BS_pt[!(is.na(BS_pt$state)),]
count(filteredBS, "state")
state_BS <- count(filteredBS, "state")

TC_pt$state <- latlong2state(TC_pt)
filteredTC <- TC_pt[!(is.na(TC_pt$state)),]
count(filteredTC, "state")
state_TC <- count(filteredTC, "state")

DT_pt$state <- latlong2state(DT_pt)
filteredDT <- DT_pt[!(is.na(DT_pt$state)),]
count(filteredDT, "state")
state_DT <- count(filteredDT, "state")

MR_pt$state <- latlong2state(MR_pt)
filteredMR <- MR_pt[!(is.na(MR_pt$state)),]
count(filteredMR, "state")
state_MR <- count(filteredMR, "state")



mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_HC$state)
dat2 <- data.frame(value = state_HC$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_HC)

mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_BS$state)
dat2 <- data.frame(value = state_BS$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_BS)


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_TC$state)
dat2 <- data.frame(value = state_TC$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_TC)


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_DT$state)
dat2 <- data.frame(value = state_DT$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
# rev(rainbow.....)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_DT)


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  state_MR$state)
dat2 <- data.frame(value = state_MR$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
# rev(rainbow.....)
spplot(USAsp['value'], col.regions= rainbow(100, start = 3/6, end = 4/6 ))
View(state_MR)





#install.packages("rworldmap")
#library(rworldmap)
library(maps)



useful_info <- c("text", "id_str", "created_at", "screen_name", "place_lat", "place_lon",  "lat", "lon", "country_code")
HC_us <- HC[HC$country_code=='US',] 
BS_us <- BS[BS$country_code=='US',]
TC_us <- TC[TC$country_code=='US',]
DT_us <- DT[DT$country_code=='US',]
MR_us <- MR[MR$country_code=='US',]

ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = BS_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = TC_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = MR_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = DT_us, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "red")

geo_pts <- c("place_lon", "place_lat")
HC_pt <-  HC_us[geo_pts]
ggplot(US_states) + geom_map(aes(map_id = region), map = US_states, fill = "grey90", color = "grey50", size = 0.25) + expand_limits(x = US_states$long, y = US_states$lat) + scale_x_continuous("Longitude") + scale_y_continuous("Latitude") + theme_minimal() + geom_point(data = HC_pt, aes(x = place_lon, y = place_lat), size = 1, alpha = 1/5, color = "blue")




# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

# Test the function using points in Wisconsin and Oregon.
#testPoints <- data.frame(x = c(-90, -120), y = c(44, 44))
#latlong2state(testPoints)

latlong2state(HC_pt)


HC_pt$state <- latlong2state(HC_pt)
filteredHC <- HC_pt[!(is.na(HC_pt$state)),]


count(filteredHC, "state")
state_HC <- count(filteredHC, "state")
View(state_HC)
write.csv(state_HC, file = "state_HC.csv",row.names=FALSE)


colwise(class)(state_HC)

#library(rgeos)
#library(maptools)
#library(gpclib)  # may be needed, may not be

# MAP
#us_state <- readShapeSpatial("/Users/alexandraplassaras/Desktop/Columbia_Courses/Spring_2016/QMSS_G4063/QMSS_G4063_Data_Visualization/assignment3/cb_2013_us_state_500k (1)/cb_2013_us_state_500k.shp")
# VERIFY IT LOADED PROPERLY
#plot(us_state)



#library(ggplot2)
#np_dist <- fortify(np_dist, region = "NAME_3")
#np_dist$id <- toupper(np_dist$id)  #change ids to uppercase
#ggplot() + geom_map(data = edu63, aes(map_id = District, fill = PASS.PERCENT), 
#                    map = np_dist) + expand_limits(x = np_dist$long, y = np_dist$lat)

#library(maps)
#library(maptools)
#library(ggplot2)
#library(ggmap)
#map.text("state", regions=c(state_HC$state), labels=as.character(state_HC$freq))





# maps per candidate by population
population <-read.csv("VotingPopulation.csv", header = TRUE, sep = ",", quote = "\"")

HC_pop <- merge(population,state_HC)
HC_pop$freq <- HC_pop$freq/HC_pop$pop
View(HC_pop)

BS_pop <- merge(population,state_BS)
BS_pop$freq <- BS_pop$freq/BS_pop$pop
View(BS_pop)

TC_pop <- merge(population,state_TC)
TC_pop$freq <- TC_pop$freq/TC_pop$pop
View(TC_pop)

MR_pop <- merge(population,state_MR)
MR_pop$freq <- MR_pop$freq/MR_pop$pop
View(MR_pop)

DT_pop <- merge(population,state_DT)
DT_pop$freq <- DT_pop$freq/DT_pop$pop
View(DT_pop)

# map time
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  HC_pop$state)
dat2 <- data.frame(value = HC_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  BS_pop$state)
dat2 <- data.frame(value = BS_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  TC_pop$state)
dat2 <- data.frame(value = TC_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  MR_pop$state)
dat2 <- data.frame(value = MR_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        


mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  DT_pop$state)
dat2 <- data.frame(value = DT_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rainbow(100, start = 3/6, end = 4/6 ))        









######################
#Extra Credit Section
######################

latlong2county <- function(pointsDF) {
  states <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=wgs84"))
  pointsSP <- SpatialPoints(pointsDF,
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  indices <- over(pointsSP, states_sp)
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

#import county list
data(county.fips)

################
################
################
################
################
################
county <- latlong2county(HC_pt)
View(all_pt)

#convert county names to fip code
fips<-with(county.fips, fips[match(county, polyname)])
US_pt1 <- US_pt
US_pt1$fips <- fips 

all_pt
#count fips
US_pt <- count(all_pt,"fips")

#Transfer into a tab seperated value file to be used in D3
write.table(US_pt1, file='UStweets.tsv', quote=FALSE, sep='\t')




#[1] "wisconsin" "oregon" # IT WORKS

#write.csv(HC_pt, file = "HC.csv")


# Part 2 - Counting tweets per state



#includeHTML - ??