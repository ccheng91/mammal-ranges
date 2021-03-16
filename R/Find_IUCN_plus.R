
# Factors affecting mismatch between IUCN range maps and camera trap occurrences of mammal

# Find species that:
# IUCN + , camera +
# IUCN + , camera -

# IUCN + , camera +
# IUCN - , camera +

#load package
rm(list=ls(all=TRUE))
library(ggplot2)
library(sf)
library(dplyr)
library(geosphere)

# Read data

TERR_mal <- st_read("data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
terr_mal_water <- st_read("data/MAMMALS_FRESHWATER/MAMMALS_FRESHWATER.shp")
terr_mal_new <- rbind(TERR_mal,terr_mal_water)

# Camera trap data  
spp_df_all <- read.csv("result/occ_dataframe_taxon_fixed.csv") 

head(spp_df_all)
# Total NO. IUCN mammal species  
length(unique(terr_mal_new$binomial))

# Load functional trait data
fun_data <- read.delim("data/MamFuncDat.txt")

head(fun_data)

# < 500g species

fun_data_500 <- fun_data %>% filter(BodyMass.Value < 500)
fun_data_more_than_500 <- fun_data %>% filter(BodyMass.Value >= 500)

# Filter range maps
# By order
terr_mal_new$order_ <- stringr::str_to_title(terr_mal_new$order_)
filter_TERR_mal <-  terr_mal_new %>% 
  filter(order_ != "Chiroptera") %>% # bats
  filter(order_ != "Scandentia") %>% # Colugos
  filter(order_ != "Macroscelidea") %>% # Shrew opossum
  filter(order_ != "Paucituberculata") %>% # elephant shrew
  filter(order_ != "Microbiotheria") %>% # colocolo opossum 1 speices
  filter(order_ != "Sirenia") %>% # manatee
  filter(order_ != "Eulipotyphla") 

# By body mass
`%notin%` <- Negate(`%in%`) # opposite of %in% 
small_mammal <- unique(fun_data_500$Scientific)

filter_TERR_mal <- filter_TERR_mal %>%
  filter(binomial %notin% small_mammal) 

# Calculate the IUCN vs camera match type
# Use TEAM data as an example

TEAM_BBS <- spp_df_all[grep("TEAMS_CT-BBS", spp_df_all$deploymentID),]

head(TEAM_BBS)

shap <- raster::shapefile("data/all_in_one_folder_OCT2019/All_area_shape_NOV2020.shp")
TEAM_shap <- shap[grep("TEAMS", shap$projectID),]
TEAM_projectID <- unique(TEAM_shap$projectID)

TEAM_BBS_shap <- shap[grep("TEAMS_BBS", shap$projectID),]

crs(filter_TERR_mal)
crs(TEAM_BBS_shap)

plot(TEAM_BBS_shap)

croped <- rgeos::intersect(filter_TERR_mal, TEAM_BBS_shap)

length(croped$binomial)
length(unique(croped$binomial))
length(unique(TEAM_BBS$speciesScientificName))


cam_spp <-  unique(TEAM_BBS$speciesScientificName)
IUCN_spp <- unique(croped$binomial)

both_have <- cam_spp[cam_spp %in% IUCN_spp] 
cam_only <- cam_spp[!(cam_spp%in%both_have)] 
IUCN_only <- IUCN_spp[!(IUCN_spp %in% both_have)]


### Factors explaining camera only

# make df



cam_only_df <- data.frame(speciesScientificName=cam_only,type="A")

both_have_df <- data.frame(speciesScientificName=both_have,type="B")

IUCN_only_df <- data.frame(speciesScientificName=IUCN_only,type="C")

modelling_df <- rbind(cam_only_df,both_have_df,IUCN_only_df)

modelling_df$projectID <- TEAM_projectID[1]

head(modelling_df)
nrow(modelling_df)

trait_data <- fun_data %>% select(speciesScientificName=Scientific,Diet.Inv,Diet.PlantO,ForStrat.Value,BodyMass.Value)

left_join(modelling_df, trait_data, by="speciesScientificName")



