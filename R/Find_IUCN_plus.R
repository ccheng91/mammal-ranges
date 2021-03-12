
# Factors affecting mismatch bwteen IUCN range map and camera trap ourrance data

# Find species that:
# IUCN + , camera +
# IUCN + , camera -

# IUCN + , camera +
# IUCN - , camera +

#load pacakge
rm(list=ls(all=TRUE))
library(ggplot2)
library(raster)
library(dplyr)
library(geosphere)

# Read data

TERR_mal <- shapefile("data/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp")
spp_df_all <- read.csv("result/occ_dataframe_taxon_fixed.csv") 

# Total NO. IUCN mammal species  
length(unique(TERR_mal$binomial))

# Load functional trait data
fun_data <- read.delim("data/MamFuncDat.txt")

head(fun_data)

# < 500g species

fun_data_500 <- fun_data %>% filter(BodyMass.Value < 500)
fun_data_more_than_500 <- fun_data %>% filter(BodyMass.Value >= 500)

# Filter range maps

filter_TERR_mal <- TERR_mal[-grep("Chiroptera", TERR_mal$order_,  ignore.case = T),]   # bats
# filter_TERR_mal <- filter_TERR_mal[-grep("Cetacea", filter_TERR_mal$order_,  ignore.case = T),]  # no whales, terrestrial only
filter_TERR_mal <- filter_TERR_mal[-grep("Scandentia", filter_TERR_mal$order_,  ignore.case = T),]  #colugos
filter_TERR_mal <- filter_TERR_mal[-grep("Macroscelidea", filter_TERR_mal$order_,  ignore.case = T),]   # shrew opossum
filter_TERR_mal <- filter_TERR_mal[-grep("Paucituberculata", filter_TERR_mal$order_,  ignore.case = T),]  # elephant shrew
filter_TERR_mal <- filter_TERR_mal[-grep("Microbiotheria", filter_TERR_mal$order_,  ignore.case = T),]  #colocolo opossum 1 speices
# filter_TERR_mal <- filter_TERR_mal[-grep("Sirenia", filter_TERR_mal$order_,  ignore.case = T),]  # no manatee, terrestrial only
filter_TERR_mal <- filter_TERR_mal[-grep("Eulipotyphla", filter_TERR_mal$order_,  ignore.case = T),] 

filter_TERR_mal <- filter_TERR_mal[!(filter_TERR_mal$binomial %in% fun_data_500$Scientific),]

#IUCN_flitered <- filter_TERR_mal$binomial[filter_TERR_mal$binomial %in% fun_data_more_than_500$Scientific]
IUCN_flitered_unique <- unique(filter_TERR_mal$binomial)
length(IUCN_flitered_unique)
which( fun_data_more_than_500$Scientific == "Canis rufus")


# Caculate the IUCN vs camera match type
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
plot(filter_TERR_mal)

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



