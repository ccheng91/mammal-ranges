#Identify species that's not matched in IUCN range map

# load data
library(sf)
library(dplyr)

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

[1] "Cercopithecus lhoesti"      "Cebus apella"               "Saguinus fuscicollis"      
[4] "Capricornis milneedwardsii" "Procolobus gordonorum"      "Lagothrix cana"            

[7] "Pekania pennanti"           "Madoqua Kirkii"             "Ardeotis kori"             
[10] "Taurotragus oryx"           "Lepus microtis"             "Castor Canadensis"         
[13] "Mustela vison"              "Tamias dorsalis"            "Camelus dromedarius"       
[16] "Naemorhedus griseus"

#### Do not run ##########
#### Name to be fixed ####
 
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Cercopithecus lhoesti" )]<- "Allochrocebus lhoesti"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Cebus apella")] <- "Sapajus apella"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Saguinus fuscicollis")] <- "Leontocebus fuscicollis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Capricornis milneedwardsii")] <- "Capricornis sumatraensis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Lagothrix cana")] <- "Lagothrix lagothricha"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Procolobus gordonorum")] <- "Piliocolobus gordonorum"

# write.csv(spp_df_all,"result/occ_dataframe_taxon_fixed.csv",row.names = F)

#### Do not run ##########
#### Name to be fixed ####

spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Pekania pennanti")] <- "Martes pennanti"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Madoqua Kirkii")] <- "Madoqua kirkii"
spp_df_all <- spp_df_all[-which(spp_df_all$speciesScientificName =="Ardeotis kori"),]

spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Taurotragus oryx")] <- "Tragelaphus oryx"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Lepus microtis")] <- "Lepus victoriae"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Castor Canadensis")] <- "Castor canadensis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Mustela vison")] <- "Neovison vison" 
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Tamias dorsalis")] <- "Neotamias dorsalis"
spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName =="Naemorhedus griseus")] <-  "Naemorhedus goral"
                                       
# write.csv(spp_df_all,"result/occ_dataframe_taxon_fixed.csv",row.names = F)    

#### Do not run ##########
 
 # March 13th updates 
 spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Erethizion dorsatum")]  <-  "Erethizon dorsatum"
 spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Damaliscus korrigum")]  <- "Damaliscus lunatus"
 spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Martes Pennanti")] <- "Martes pennanti"
 spp_df_all$speciesScientificName[which(spp_df_all$speciesScientificName == "Galagoides demidovii")] <- "Galagoides demidoff"

# write.csv(spp_df_all,"result/occ_dataframe_taxon_fixed.csv",row.names = F)

 
# Old fix
# Fix wrong names
 
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
  filter( speciesScientificName !=  "Sagittarius serpentarius")



write.csv(spp_df_all,"result/occ_dataframe_taxon_fixed.csv",row.names = F)


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







