# Caculate distance from the project to the range edge
library(sf)
type_a <-  df %>% filter(type == "A") 

head(type_a)

type_a$speciesScientificName[1]
type_a$projectID[1]


TERR_mal <- st_read("data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
terr_mal_water <- st_read("data/MAMMALS_FRESHWATER/MAMMALS_FRESHWATER.shp")
terr_mal_new <- rbind(TERR_mal,terr_mal_water) %>% filter(presence == 1) 

shap <- st_read("data/all_in_one_folder_OCT2019/All_area_shape_NOV2020.shp")


distance <- type_a %>% dplyr::select(speciesScientificName,projectID)

for (i in 1:nrow(type_a)) {
  
  # extract ploygon
  poly1 <- terr_mal_new[terr_mal_new$binomial == type_a$speciesScientificName[i],]
  poly2 <- shap[shap$projectID==type_a$projectID[i],]

  # create an index of the nearest feature
  index <- st_nearest_feature(x = poly1, y = poly2)
  
  # slice based on the index
  poly2 <- poly2 %>% slice(index)
  
  # calculate distance between polygons
  poly_dist <- st_distance(x = poly1, y= poly2, by_element = TRUE)
  
  min_distance <- min(poly_dist)
  
  distance[i,3] <- min_distance
}

names(distance)[3] <- "distance"
#write.csv(distance,"result/4-minimal_distance_out_spp.csv",row.names = F)

distance <- read.csv("result/4-minimal_distance_out_spp.csv")


distance$distance <- distance$distance/1000

png(filename = "plot/minimal_distance.png",width = 480, height = 480)

hist(round(distance$distance,digits = 0),breaks=100, main="Histogam of minimal distance",
     xlab="Distance (KM)")

dev.off()

length(which(distance$distance<100))

more_2000 <- distance[which(distance$distance>2000),]

unique(more_2000$speciesScientificName)
