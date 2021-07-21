# Add covariates
library(dplyr)

# load df
# modelling_df <- read.csv("result/modeling_df.csv")
modelling_df <- read.csv("result/June2021/1.0-modeling_df_add_emml_etc.csv")

which(modelling_df$speciesScientificName == "All_in")
# add species traits
# Eltonian traits

fun_data <- read.delim("data/MamFuncDat.txt")
Panth_data <- read.delim("data/ECOL_90_184/PanTHERIA_1-0_WR05_Aug2008.txt")

head(fun_data)
str(Panth_data)
# 1. body mass. 2. Foraging stratum 4. nocturnal 5. diet breadth. 6 Group size

# diet breadth
diet_matrix <- fun_data[,4:13] 
diet.breadth <- rowSums(diet_matrix != 0)
fun_data$diet.breadth <- diet.breadth

elton_trait <- fun_data %>% dplyr::select(Scientific,ForStrat.Value,diet.breadth,BodyMass.Value, 
                                Activity.Nocturnal,Activity.Diurnal)


# large group sized 
Panth_trait <- Panth_data %>%  dplyr::select(Scientific=MSW05_Binomial,X15.1_LitterSize,
                                            X10.1_PopulationGrpSize,X10.2_SocialGrpSize)

# No-matches of two trait base
length(which(Panth_trait$Scientific %in% elton_trait$Scientific == F))
# 20 species

speices_trait <- left_join(elton_trait,Panth_trait,by="Scientific")
str(speices_trait)

# combine group size
# Remove four NA species, three small, one dolphin
speices_trait <- speices_trait[-which(is.na(speices_trait$X10.1_PopulationGrpSize)),]
for (i in 1:nrow(speices_trait)) {
 if(speices_trait$X10.1_PopulationGrpSize[i] != -999 & speices_trait$X10.2_SocialGrpSize[i]== -999){
    speices_trait$X10.2_SocialGrpSize[i] <- speices_trait$X10.1_PopulationGrpSize[i]}
}

head(modelling_df)
names(speices_trait)[1] <- "speciesScientificName"


# First left join 

modelling_df_add_trait <- left_join(modelling_df,speices_trait,by="speciesScientificName")

## Species that have no match in trait database
missing_trait <- modelling_df_add_trait[is.na(modelling_df_add_trait$ForStrat.Value),]


##################################
## From here -> Taxa_clean_up.R ##
################################## 
library(taxadb)

# add anchor for missing trait species 
names <- data.frame(Scientific=unique(unique(missing_trait$speciesScientificName)))
names <- names %>% mutate(id = get_ids(Scientific, "itis")) %>% 
  mutate(accepted_name = get_names(id, "itis"))

names <- names %>% mutate(id_wd = get_ids(Scientific, "wd",version=2019)) %>% 
  mutate(accepted_name_wd = get_names(id_wd, "wd",version=2019))

# add anchor for elton trait species 
elton_mam <- speices_trait %>% dplyr::select(Scientific=speciesScientificName ) %>% mutate(id = get_ids(Scientific, "itis")) %>% 
  mutate(accepted_name = get_names(id, "itis"))

need_filter_el <- taxadb::filter_name("Spermophilus saturatus", "itis") %>% 
  mutate(acceptedNameUsage = get_names(acceptedNameUsageID)) %>% 
  select(scientificName, taxonomicStatus, acceptedNameUsage, acceptedNameUsageID)

elton_mam[elton_mam$Scientific == "Spermophilus saturatus",]
elton_mam[elton_mam$Scientific == "Spermophilus saturatus",2] <- need_filter_el$acceptedNameUsageID[1]
elton_mam[elton_mam$Scientific == "Spermophilus saturatus",3] <- need_filter_el$acceptedNameUsage[1]
elton_mam[elton_mam$Scientific == "Spermophilus saturatus",]

elton_mam <- elton_mam  %>% mutate(id_wd = get_ids(Scientific, 'wd',version = 2019)) %>% 
  mutate(accepted_name_wd = get_names(id_wd, 'wd',version = 2019))

head(elton_mam)
head(names)

# Replace NA with -99 and -999

names$id[is.na(names$id)] <- -99
names$accepted_name[is.na(names$accepted_name)] <- -99
names$id_wd[is.na(names$id_wd)] <- -99
names$accepted_name_wd[is.na(names$accepted_name_wd)] <- -99

# Replace NA with -99 and -999

elton_mam$id[is.na(elton_mam$id)] <- -999
elton_mam$accepted_name[is.na(elton_mam$accepted_name)] <- -999
elton_mam$id_wd[is.na(elton_mam$id_wd)] <- -999
elton_mam$accepted_name_wd[is.na(elton_mam$accepted_name_wd)] <- -999

# See if the taxadb can fix some problem
nrow(names)

# number of species fixed 
length(which(names$id %in% elton_mam$id))
# 27

head(names)
head(elton_mam)

# make anchor 1
ind <- which(names$id %in% elton_mam$id)

anchor_1 <- names %>% left_join(elton_mam,by="id") %>% dplyr::slice(ind) %>% dplyr::select(Scientific=Scientific.x,anchor=Scientific.y)

###########################################
## check IUCN synonyms and make anchor 2 ##
###########################################

names_2nd <- names[-ind,]

# check if wd database fix any names?
length(which(names_2nd$id_wd %in% elton_mam$id_wd))
which(names_2nd$id_wd %in% elton_mam$id_wd)
# NOPE !
names_2nd


head(names_2nd) 

# search for IUCN synonyms and see if it's in the trait database

for (i in 1:nrow(names_2nd)) {
  IUCN_result <- rredlist::rl_synonyms(names_2nd$Scientific[i],key="29c88e9e867726644b28997693189b9a9301b21e4bdf0280d51b3006bebc6642")
  IUCN_syn <-  IUCN_result$result$synonym[1]
  if(IUCN_result$count != 0){
    names_2nd$IUCN_syn[i] <- IUCN_syn
  } else {
    names_2nd$IUCN_syn[i] <- "No_IUCN_synonym" 
  }
}

# number of name fixed by IUCN
length(which(names_2nd$IUCN_syn %in% elton_mam$Scientific == T))
length(which(names_2nd$IUCN_syn %in% elton_mam$accepted_name == T))
# 27

names_2nd$IUCN_syn_in_trait_data <- names_2nd$IUCN_syn %in% elton_mam$Scientific

ind_2 <-  which(names_2nd$IUCN_syn_in_trait_data == T)
names_2nd$accepted_name[(names_2nd$IUCN_syn_in_trait_data == T)] <- names_2nd$IUCN_syn[(names_2nd$IUCN_syn_in_trait_data == T)] 


anchor_2 <- names_2nd %>% left_join(elton_mam,by="accepted_name") %>% dplyr::slice(ind_2) %>% 
                          dplyr::select(Scientific=Scientific.x,anchor= accepted_name)

# Load checked anchor names 
anchor_3 <- read.csv("result/anchor_names_for_spps_found_no_traits.csv")


head(anchor_1)
head(anchor_2)
head(anchor_3)

anchor_names <- rbind(anchor_1,anchor_2,anchor_3)

nrow(anchor_names)
nrow(names)

head(anchor_names)
head(missing_trait)

names(anchor_names) <- c("speciesScientificName","anchor")

missing_trait_filling <- missing_trait %>% left_join(anchor_names,by="speciesScientificName") %>% 
                                           dplyr::select(orignial_names=speciesScientificName, type, projectID, speciesScientificName=anchor)
head(missing_trait_filling)
head(speices_trait)

# Second left_join
# 
# final = rbind (correct "missing_trait_filling" + remaining)
#

missing_trait_filling <- missing_trait_filling %>% left_join(speices_trait,by="speciesScientificName") 

missing_trait_filling <- missing_trait_filling[, -4]
names(missing_trait_filling)[1] <- "speciesScientificName"

remaining <- modelling_df_add_trait %>% filter(!is.na(ForStrat.Value)) 

str(missing_trait_filling)

str(remaining)

modelling_df_final <- rbind(remaining, missing_trait_filling)

head(modelling_df_final)

#write.csv(modelling_df_final,"result/2.0-modelling_df_with_traits.csv",row.names = F)
write.csv(modelling_df_final,"result/June2021/2.0-modelling_df_with_traits_emml_etc_added.csv",row.names = F)




















