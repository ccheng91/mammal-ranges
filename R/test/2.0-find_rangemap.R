library(ggplot2)
library(raster)
library(dplyr)
library(geosphere)

#TERR_mal <- shapefile("data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
TERR_mal <- shapefile("data/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp")
spp_df_all <- read.csv("result/occ_dataframe.csv")

head(spp_df_all)

spp_df_all_ori <- spp_df_all

spp_df_all <- spp_df_all_ori %>% filter(!is.na(Latitude))

all_spp <- unique(spp_df_all$speciesScientificName)

# vaild_itis_spp <- read.csv("result/vaild_itis_spp.csv")
# head(vaild_itis_spp)

length(TERR_mal$binomial[which(all_spp %in% TERR_mal$binomial)])

###

# Mean lat and long
mlong <- mean(spp_gps$x)
mlat <- mean(spp_gps$y)

# UTM finder function
lonlat2UTM <-  function(lonlat) {
  utm <-  (floor((lonlat[1] + 180) / 6) %% 60) + 1
  if(lonlat[2] > 0) {
    utm + 32600
  } else{
    utm + 32700
  }
}

# Find UTM
projutm <- lonlat2UTM(c(mlong,mlat))

### plot occ data on rangemap 
assert_spp_point <- data.frame(9999,9999,9999,9999,9999,9999,9999)
names(assert_spp_point) <- c("speciesScientificName","occ_NO","in_range_NO","order","family","IUCN","Distance")
for (i in 1:length(all_spp)) {
  one_spp_occ <- spp_df_all[which(spp_df_all$speciesScientificName == all_spp[i]),]
  
  if(all_spp[i] %in% TERR_mal$binomial){
  
    one_spp_range <- TERR_mal[TERR_mal$binomial == all_spp[i],]
    proj <- proj4string(one_spp_range)
    spp_gps <- data.frame(y=one_spp_occ$Latitude, x=one_spp_occ$Longitude)
    coordinates(spp_gps) <- c("x", "y")
    proj4string(spp_gps) <- CRS(proj)
    spp_gps <- spTransform(spp_gps, proj)
    spp_in <- spp_gps[one_spp_range,]
    occ_NO <- length(spp_gps)
    in_range_NO <- length(spp_in)
  
    #####
    # Distance
    mlong <- mean(spp_gps$x)
    mlat <- mean(spp_gps$y)
    
    projutm <- lonlat2UTM(c(mlong,mlat))
    projutm <- sf::st_crs(projutm)$proj4string
    
    proj4string(one_spp_range)  <-  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    spsutm <- spTransform(one_spp_range,projutm)
    
    spp_gps <- data.frame( x=one_spp_occ$Longitude,y=one_spp_occ$Latitude)
    
    spp_gps <- SpatialPoints(spp_gps, proj4string=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    spp_gps <- spTransform(spp_gps, projutm)
    
    spsutm <- spTransform(one_spp_range,projutm)
    min_dis <- apply(rgeos::gDistance(spp_gps, spsutm,byid=TRUE),2,min)
    mean_dis <- mean(min_dis[which(min_dis != 0)])
    #####
    
    # add data
    assert_spp_point[i,1] <- all_spp[i]
    assert_spp_point[i,2] <- occ_NO
    assert_spp_point[i,3] <- in_range_NO
    assert_spp_point[i,4] <- unique(one_spp_range$order_)
    assert_spp_point[i,5] <- unique(one_spp_range$family)
    assert_spp_point[i,6] <- unique(one_spp_range$category)
    assert_spp_point[i,7] <- mean_dis
    }else{
      assert_spp_point[i,1] <- all_spp[i]
      assert_spp_point[i,2] <- nrow(one_spp_occ)
      assert_spp_point[i,3] <- "NA"
      assert_spp_point[i,4] <- "NA"
      assert_spp_point[i,5] <- "NA"
      assert_spp_point[i,6] <- "NA"
      assert_spp_point[i,7] <- "NA"
    }
  }

assert_spp_point$present <- as.numeric(assert_spp_point$in_range_NO)/as.numeric(assert_spp_point$occ_NO)

hist(as.numeric(assert_spp_point$Distance))

# sf_gps <- sf::st_as_sf(spp_gps) 
# sf_range <- sf::st_as_sf(one_spp_range) 
# sf::st_distance(sf_gps,sf_range,by_element=T)

spp_point_complet <- assert_spp_point %>% filter(in_range_NO!="NA")
spp_point_incomplet <- assert_spp_point %>% filter(in_range_NO=="NA")

nrow(spp_point_complet)
nrow(spp_point_incomplet)

mean(spp_point_complet$present,na.rm=T)

hist(assert_spp_point$present,main = "Histogram of persentage", xlab = "")

# all other 
hundred <- spp_point_complet[spp_point_complet$present==1,]
between_0_1 <- spp_point_complet[spp_point_complet$present > 0 & spp_point_complet$present <1,]
zero <- spp_point_complet[spp_point_complet$present<=0,]
non_hundred <- spp_point_complet[spp_point_complet$present!=1,]

hist(non_hundred$present)

#### Find which continent and country

library(sp)
library(rworldmap)

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coords2continent = function(points)
{  
  countriesSP <- getMap(resolution='low')
  #countriesSP <- getMap(resolution='high') #you could use high res map from rworldxtra if you were concerned about detail
  
  # converting points to a SpatialPoints object
  # setting CRS directly to that from rworldmap
  pointsSP = SpatialPoints(points, proj4string=CRS(proj4string(countriesSP)))  
  
  
  # use 'over' to get indices of the Polygons object containing each point 
  indices = over(pointsSP, countriesSP)
  
  #indices$continent   # returns the continent (6 continent model)
  indices$REGION   # returns the continent (7 continent model)
  #indices$ADMIN  #returns country name
  #indices$ISO3 # returns the ISO3 code 
}

points = data.frame(lon=spp_df_all$Longitude, lat=spp_df_all$Latitude)
spp_df_all$continent <-  as.character(coords2continent(points))

table(spp_df_all$continent)

# check if 1 spp in two continent

check_continent <-unique(data.frame(speciesScientificName=spp_df_all$speciesScientificName,
                                    continent=spp_df_all$continent))

occ_Freq <- table(check_continent$speciesScientificName)
head(check_continent)

check_continent[duplicated(check_continent$speciesScientificName),]

spp_df_all$continent[which(is.na(spp_df_all$continent))] <- "North America"

check_continent$continent[which(is.na(check_continent$continent))] <- "North America"
check_continent <-unique(data.frame(speciesScientificName=spp_df_all$speciesScientificName,
                                    continent=spp_df_all$continent))
check_continent[duplicated(check_continent$speciesScientificName),]


####################
##### plot ########
####################

one_spp_occ <- spp_df_all[which(spp_df_all$speciesScientificName == "Sus scrofa"),]
one_spp_range <- TERR_mal[TERR_mal$binomial=="Sus scrofa",]
gps <- data.frame(y=one_spp_occ$Latitude, x=one_spp_occ$Longitude)
coordinates(gps) <- c("x", "y")
proj4string(gps) <- CRS(proj)
gps <- spTransform(gps, proj)

plot(one_spp_range, xlim=c(90,110) , ylim=c(-10,40), col="steelblue", lwd=0.5,
     main= "Sus scrofa")

points(gps, col="firebrick", pch = 3)

####################
one_spp_occ <- spp_df_all[which(spp_df_all$speciesScientificName == "Hippopotamus amphibius"),]
one_spp_range <- TERR_mal[TERR_mal$binomial=="Hippopotamus amphibius",]
gps <- data.frame(y=one_spp_occ$Latitude, x=one_spp_occ$Longitude)
coordinates(gps) <- c("x", "y")
proj4string(gps) <- CRS(proj)
gps <- spTransform(gps, proj)

plot(one_spp_range, xlim=c(35,40) , ylim=c(-10,10), col="steelblue", lwd=0.5,
     main= "Hippopotamus amphibius (0)")

points(gps, col="firebrick", pch = 3)

####################
one_spp_occ <- spp_df_all[which(spp_df_all$speciesScientificName == "Rusa unicolor"),]
one_spp_range <- TERR_mal[TERR_mal$binomial=="Rusa unicolor",]
gps <- data.frame(y=one_spp_occ$Latitude, x=one_spp_occ$Longitude)
coordinates(gps) <- c("x", "y")
proj4string(gps) <- CRS(proj)
gps <- spTransform(gps, proj)

plot(one_spp_range, col="steelblue", lwd=0.5,
     main= "Rusa unicolor (0.73)")

points(gps, col="firebrick", pch = 3)

####################
one_spp_occ <- spp_df_all[which(spp_df_all$speciesScientificName == "Crossarchus obscurus"),]
one_spp_range <- TERR_mal[TERR_mal$binomial=="Crossarchus obscurus",]
gps <- data.frame(y=one_spp_occ$Latitude, x=one_spp_occ$Longitude)
coordinates(gps) <- c("x", "y")
proj4string(gps) <- CRS(proj)
gps <- spTransform(gps, proj)

plot(one_spp_range, xlim=c(-13.727,9) , col="steelblue", lwd=0.5,
     main= "Crossarchus obscurus (0)")

points(gps, col="firebrick", pch = 3)

##################
# Modelling the range fit
head(spp_point_complet)

spp_point_complet <- left_join(spp_point_complet, check_continent,by="speciesScientificName")

table(spp_point_complet$order,spp_point_complet$present)
table(spp_point_complet$continent)

table(spp_df_all$continent)

spp_point_complet %>% group_by(continent) %>% summarise(mean(present))

table(spp_point_complet$IUCN)

table(spp_df_all$IUCN)

spp_df_all_IUCN <- spp_df_all %>% left_join(check_IUCN,by="speciesScientificName")
spp_df_all_info <- spp_df_all %>% left_join(spp_point_complet,by="speciesScientificName")

head(spp_df_all)

head(spp_point_complet)
check_IUCN <- data.frame(speciesScientificName=spp_point_complet$speciesScientificName,IUCN=spp_point_complet$IUCN)
head(check_IUCN)

table(spp_df_all_IUCN$IUCN)

spp_point_complet %>% group_by(IUCN) %>% summarise(mean(present))

spp_point_complet$order <- stringr::str_to_title(spp_point_complet$order)
par(mar=c(8,8,1,1))
dat <- table(spp_point_complet$order)
order(dat)
barplot(table(spp_point_complet$order),las = 3,width=3)

barplot(table(spp_df_all_info$order),las = 3,width=3)

#########################
names(spp_point_complet)[8] <- "Range_fit"

summary(lm(Range_fit ~ occ_NO + continent, data=spp_point_complet))

spp_point_complet_NAN <- spp_point_complet %>% filter(Distance !=  NaN)

spp_point_complet_NAN$Distance <- scale(as.numeric(spp_point_complet_NAN$Distance))
summary(lm(Distance ~ occ_NO + continent, data=spp_point_complet_NAN))
head(spp_point_complet)
