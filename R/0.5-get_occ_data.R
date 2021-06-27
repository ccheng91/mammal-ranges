library(dplyr)
library(sf)
library(lubridate)

rm(list=ls(all=TRUE))

## make occurrence record data
spp_path <- list.files("meta_database/Image/Image_filtered",
                       ignore.case = TRUE, full.names=TRUE, recursive=TRUE)
team_path <- spp_path[4:19]
head(team_path)

# Note:
# image:deploymentID = deployment:deploymentLocationID

dep_team <- read.csv("meta_database/Deployment/Deployment_TEAMS.csv",
                     stringsAsFactors = F, header = T)

str(dep_team)

dep_3 <- dep_team %>% dplyr::select(projectID=projectID, deploymentID=deploymentLocationID,Latitude, Longitude) %>% unique()

spp_df <- data.frame()      

for(i in 1:length(team_path)){
  image_temp <- read.csv(team_path[i])
  image_temp <- image_temp %>% dplyr::select(deploymentID,dateTimeCaptured,speciesScientificName) %>% 
                               mutate(date=ymd_hms((dateTimeCaptured))) %>% mutate(year=year(date)) %>% 
                               dplyr::select(deploymentID,speciesScientificName,year) %>% unique()
  new <- left_join(image_temp ,dep_3, by="deploymentID") %>% unique()
  spp_df <- rbind(spp_df,new)
}


# non-team, non-emmal project

gc <- read.csv(spp_path[1])
ITBD <- read.csv(spp_path[2])
SRGT <- read.csv(spp_path[3])

dep_pro1 <- read.csv("meta_database/Deployment/Deployment_GC.csv")
dep_pro2 <- read.csv("meta_database/Deployment/Deployment_ITBD.csv")
dep_pro3 <- read.csv("meta_database/Deployment/Deployment_SRGT.csv")

str(dep_pro1)
str(dep_pro2)
str(dep_pro3)

dep_others <- rbind(dep_pro1,dep_pro2)

dep_others <- dep_others %>% dplyr::select(projectID,deploymentID=deploymentLocationID, Latitude, Longitude) %>% unique()

dep_pro_srgt <- dep_pro3 %>% dplyr::select(projectID,deploymentID=deploymentID, Latitude, Longitude) %>% unique()

dep_others <- rbind(dep_others,dep_pro_srgt)
  
spp_df2 <- data.frame()

for(i in 1:3){
  image_temp <- read.csv(spp_path[i])
  image_temp <- image_temp %>% dplyr::select(deploymentID,dateTimeCaptured,speciesScientificName) %>% 
    mutate(date=as.Date((dateTimeCaptured))) %>% mutate(year=year(date)) %>% 
    dplyr::select(deploymentID,speciesScientificName,year) %>% unique()
  new <- left_join(image_temp ,dep_others, by="deploymentID") %>% unique()
  spp_df2 <- rbind(spp_df2,new)
}

str(spp_df2)
table(spp_df2$projectID)

# Emammal 

emmal_path <- spp_path[20:21]

dep_path_emammal_US <- list.files("meta_database/Emml_redownload_NOV2019/all_deployment", 
                              pattern = "\\.(csv)$", ignore.case = TRUE,full.names=TRUE, recursive=TRUE)

listdeply_emammal_us <- list()
for(i in 1:length(dep_path_emammal_US)){
  llll <- read.csv(dep_path_emammal_US[i], stringsAsFactors = F, header = T)
  listdeply_emammal_us[[i]] <- llll
}

deply_emammal_us <- do.call("rbind", listdeply_emammal_us)
deply_emammal_us <- unique(deply_emammal_us)

emml_US <- read.csv(spp_path[21],stringsAsFactors = F, header = T)

head(deply_emammal_us)
head(emml_US)

##

  image_temp <- emml_US %>% dplyr::select(deploymentID,imageSequenceBeginTime,speciesScientificName) %>% 
    mutate(date=as.Date((imageSequenceBeginTime))) %>% mutate(year=year(date)) %>% 
    dplyr::select(deploymentID,speciesScientificName,year) %>% unique()

### Making right deploymentID for emml_dep_US
### Match project name and attach EMML_abbr_ to deployment ID
  
head(deply_emammal_us)
  
site_info_emml <- read.csv("meta_database/Emml_redownload_NOV2019/project_info_US_redownloaded_NOV2019.csv", 
                  stringsAsFactors = F,fileEncoding="UTF-8-BOM")
  
correct_BE_time <- deply_emammal_us %>% dplyr::select(projectID=project_id, deployment_id,actual_date_out,retrieval_date,project_name,
                                                 sub_project_name,camera_failure_details,quiet_period_setting,actual_lat, actual_long)
  
  head(site_info_emml)
  head(correct_BE_time)
  names(site_info_emml)
  
### add the abbr to deplyment ID
  for (i in 1:nrow(correct_BE_time)) {
    abbr_ind <- site_info_emml$abbr[correct_BE_time$projectID[i] == site_info_emml$project_id]
    if(length(abbr_ind)==1){
      correct_BE_time$projectID[i] <- abbr_ind
      abbr_ind <- paste(abbr_ind, correct_BE_time$deployment_id[i],sep ="_")
      correct_BE_time$deployment_id[i] <- abbr_ind}
    if(length(abbr_ind) == 2 ){
      sub_ind  <- deply_emammal_us$sub_project_id[i]
      if(sub_ind == "sp1302"){
        correct_BE_time$projectID[i] <- "EMML_OPRI"
        abbr_ind <- paste("EMML_OPRI", correct_BE_time$deployment_id[i],sep ="_")
        correct_BE_time$deployment_id[i] <- abbr_ind
      } else{
        correct_BE_time$projectID[i] <- "EMML_CHIR"
        abbr_ind <- paste("EMML_CHIR", correct_BE_time$deployment_id[i],sep ="_")
        correct_BE_time$deployment_id[i] <- abbr_ind
      }
    }}
  
  
deply_emammal_us <- correct_BE_time %>% dplyr::select(deploymentID=deployment_id,Latitude=actual_lat, Longitude=actual_long) %>%
    unique()

 new <- left_join(image_temp ,deply_emammal_us, by="deploymentID") %>% unique()

 spp_df3 <- data.frame()
 spp_df3 <- rbind(spp_df3,new)
 spp_df3 <- spp_df3 %>% mutate(projectID=stringr::str_extract(deploymentID, "[^_]*_[^_]*")) %>% 
                       dplyr::select(deploymentID,speciesScientificName,year,projectID, Latitude,Longitude)
str(spp_df2)
str(spp_df3)

### Emammal other #####
 emml_other_temp <- read.csv(spp_path[20],stringsAsFactors = F, header = T)
 
 dep_emml <- read.csv("meta_database/Deployment/Deployment_EMML.csv", 
                    stringsAsFactors = F, header = T)
  head(emml_other_temp)
  head(dep_emml)
 
 dep_emml_other <- dep_emml %>% dplyr::select(deploymentID,Latitude, Longitude)
 
 image_temp <- emml_other_temp %>% dplyr::select(deploymentID,imageSequenceBeginTime,speciesScientificName) %>% 
   mutate(date=as.Date((imageSequenceBeginTime))) %>% mutate(year=year(date)) %>% 
   dplyr::select(deploymentID,speciesScientificName,year) %>% unique()

  spp_df4 <- left_join(image_temp ,dep_emml_other, by="deploymentID") %>% unique() %>% 
    mutate(projectID=stringr::str_extract(deploymentID, "[^_]*_[^_]*")) %>% 
    dplyr::select(deploymentID,speciesScientificName,year,projectID, Latitude,Longitude)
 
   str(spp_df4)
  table(spp_df4$projectID)
  
## add XSBN data

XSBN <- read.csv("data/cameratrap/xsbn.csv",fileEncoding="UTF-8-BOM")
XSBN_effort <- read.csv("data/cameratrap/XSBN_efforts.csv",fileEncoding="UTF-8-BOM")
  
head(XSBN)  
head(XSBN_effort)

XSBN_dep <- XSBN_effort %>% dplyr::select(projectID=PA, deploymentID=NO, Longitude=GPS_X, Latitude=GPS_Y)

XSBN_image <- XSBN %>% dplyr::select(deploymentID=camera, dateTimeCaptured=datetime,speciesScientificName=scientfic_name) %>% 
  mutate(date=as.Date((dateTimeCaptured))) %>% mutate(year=year(date)) %>% 
  dplyr::select(deploymentID,speciesScientificName,year) %>% unique()                     
XSBN_image$deploymentID <- toupper(XSBN_image$deploymentID)

spp_df5 <- left_join(XSBN_image ,XSBN_dep, by="deploymentID") %>% unique()

head(spp_df)
head(spp_df2)
head(spp_df3)
head(spp_df4)
head(spp_df5)

## Final spp dataframe

spp_df_all <- rbind(spp_df,spp_df2,spp_df3,spp_df4,spp_df5)

write.csv(spp_df_all,"result/0.5-occ_dataframe_with_projectID_26June21.csv",row.names = F)

#Identify species that's not matched in IUCN range map

# load data


# Camera trap species 
spp_df_all <- read.csv("result/occ_dataframe_taxon_fixed.csv") 
spp_df_all_projID <- read.csv("result/occ_dataframe_with_projectID.csv")


# Range of all Terrestrial mammal downloaded in 2017
#TERR_mal <- st_read("data/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp")

# Range of Terrestrial only mammal downloaded in 2021
terr_mal_new <- st_read("data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")

# Range of freshwater mammal downloaded in 2021
# This can be download at (https://www.iucnredlist.org/resources/spatial-data-download)
terr_mal_water <- st_read("data/MAMMALS_FRESHWATER/MAMMALS_FRESHWATER.shp")


###### test

all_speices <- unique(spp_df_all_projID$speciesScientificName) # Camera detected species

all_mammal_New <- unique(terr_mal_new$binomial) #2020
all_mammal_water <- unique(terr_mal_water$binomial) # water

all_mammal <- unique(c(all_mammal_New,all_mammal_water))

#
not_2020 <- all_speices[all_speices %in% all_mammal == F]


not_2020
length(not_2020)

not_2020


####### test


# Step 1. - match names

all_speices <- unique(spp_df_all$speciesScientificName) # Camera detected species

all_mammal_New <- unique(terr_mal_new$binomial) #2020
all_mammal_water <- unique(terr_mal_water$binomial) # water

#
not_2020 <- all_speices[all_speices %in% all_mammal_New == F]
not_2020_2 <- not_2020[not_2020 %in% all_mammal_water == F]

not_2020_2
length(not_2020_2)
# Which isn't in IUCN 

not_match_spp <- all_speices[all_speices %in% all_mammal == F]

not_match_spp_2 <- not_match_spp[not_match_spp %in% all_mammal_New == F]

not_match_spp_2
length(not_match_spp_2)
#### Do not run ##########
#### Name to be fixed ####

spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Cercopithecus lhoesti" )]<- "Allochrocebus lhoesti"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Cebus apella")] <- "Sapajus apella"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Saguinus fuscicollis")] <- "Leontocebus fuscicollis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Capricornis milneedwardsii")] <- "Capricornis sumatraensis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Lagothrix cana")] <- "Lagothrix lagothricha"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Procolobus gordonorum")] <- "Piliocolobus gordonorum"

spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Pekania pennanti")] <- "Martes pennanti"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Madoqua Kirkii")] <- "Madoqua kirkii"
spp_df_all <- spp_df_all[-which(spp_df_all$speciesScientificName =="Ardeotis kori"),]

spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Taurotragus oryx")] <- "Tragelaphus oryx"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Lepus microtis")] <- "Lepus victoriae"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Castor Canadensis")] <- "Castor canadensis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Mustela vison")] <- "Neovison vison" 
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Tamias dorsalis")] <- "Neotamias dorsalis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Naemorhedus griseus")] <-  "Naemorhedus goral"

spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Erethizion dorsatum")]  <-  "Erethizon dorsatum"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Damaliscus korrigum")]  <- "Damaliscus lunatus"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Martes Pennanti")] <- "Martes pennanti"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Galagoides demidovii")] <- "Galagoides demidoff"

spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Pardofelis temminckii")]  <-  "Catopuma temminckii"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Aonyx cinerea")]  <-  "Aonyx cinereus"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Puma yagouaroundi")]  <-  "Herpailurus yagouaroundi"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Giraffa tippelskirchi")]  <-  "Giraffa camelopardalis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Senegal bushbaby")]  <-  "Galago senegalensis"

# Wrong spp.
spp_df_all <- spp_df_all %>% 
  filter(!grepl("unknown",speciesScientificName)) %>% 
  filter( speciesScientificName != "Canis lupus familiaris") %>% 
  filter( speciesScientificName != "Domesticated cow")  %>% 
  filter( speciesScientificName != "Domesticated dog") %>% 
  filter( speciesScientificName !=  "Bos primigenius")  %>% 
  filter( speciesScientificName !=  "Marmota") %>% 
  filter( speciesScientificName !=  "Rodentia") %>%
  filter( speciesScientificName !=  "Bos frontalis") %>%
  filter( speciesScientificName !=  "Peromyscus sp.") %>% 
  filter( speciesScientificName !=  "Sagittarius serpentarius")%>%
  filter( speciesScientificName !=  "Camelus dromedarius")

##

write.csv(spp_df_all,"result/0.5-occ_dataframe_taxon_fixed_26June21.csv",row.names = F)


## Everything name should sync with IUCN name Including Elton & PanTHERIA name 

# need a function that everytime that there's a no match 

# Make a taxa link table

All_IUCN_names <-  data.frame(Scientific=unique(c(terr_mal_new$binomial, terr_mal_water$binomial)))

All_IUCN_names <- All_IUCN_names %>% mutate(id = get_ids(Scientific, "itis")) %>% 
  mutate(accepted_name = get_names(id, "itis"))

All_IUCN_names <- All_IUCN_names %>% mutate(id_wd = get_ids(Scientific, "wd", version=2019)) %>%
  mutate(accepted_name_wd = get_names(id_wd, "wd", version=2019))

head(All_IUCN_names)

# Fix names more than one match for ITIS
need_check <- filter_name('Camelus ferus','itis')  %>%
  mutate(acceptedNameUsage = get_names(acceptedNameUsageID)) %>% 
  select(scientificName, taxonomicStatus, acceptedNameUsage, acceptedNameUsageID)

head(need_check)

All_IUCN_names[All_IUCN_names$Scientific == 'Camelus ferus',]
All_IUCN_names[All_IUCN_names$Scientific == 'Camelus ferus',2] <- need_check$acceptedNameUsageID[1]
All_IUCN_names[All_IUCN_names$Scientific == 'Camelus ferus',3] <- need_check$acceptedNameUsage[1]

# Fix names more than one match for WD

need_check_2 <- filter_name(c('Baiyankamys shawmayeri','Baiyankamys habbema'),'wd',version = 2019) %>%
  mutate(acceptedNameUsage = get_names(acceptedNameUsageID,'wd',version = 2019)) %>% 
  select(scientificName, taxonomicStatus, acceptedNameUsage, acceptedNameUsageID)

head(need_check_2)

All_IUCN_names[All_IUCN_names$Scientific == 'Baiyankamys shawmayeri',]
All_IUCN_names[All_IUCN_names$Scientific == 'Baiyankamys shawmayeri',4] <- need_check_2$acceptedNameUsageID[1]
All_IUCN_names[All_IUCN_names$Scientific == 'Baiyankamys shawmayeri',5] <- need_check_2$acceptedNameUsage[1]

All_IUCN_names[All_IUCN_names$Scientific == 'Baiyankamys habbema',]
All_IUCN_names[All_IUCN_names$Scientific == 'Baiyankamys habbema',4] <- need_check_2$acceptedNameUsageID[3]
All_IUCN_names[All_IUCN_names$Scientific == 'Baiyankamys habbema',5] <- need_check_2$acceptedNameUsage[3]










