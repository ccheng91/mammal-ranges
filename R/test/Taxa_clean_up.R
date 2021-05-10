#####################################
#        Check Species Names        #
#####################################

library(taxadb)
library(tidyverse)

td_create(c("itis","col","iucn"))
td_create("iucn",version = "2019", overwrite = T)

##############################
### Add anchor ID for IUCN ###
#############################

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

########################################
### end of adding anchor ID for IUCN ###
########################################

##################################################
### end of adding anchor ID for trait database ###
##################################################

elton_data <- read.delim("data/MamFuncDat.txt")
panth_data <- read.delim("data/ECOL_90_184/PanTHERIA_1-0_WR05_Aug2008.txt")
head(elton_data)
head(panth_data)

## Elton trait database
elton_mam <- elton_trait %>% dplyr::select(Scientific) %>% mutate(id = get_ids(Scientific, "itis")) %>% 
                             mutate(accepted_name = get_names(id, "itis"))

need_filter_el <- taxadb::::filter_name("Spermophilus saturatus", "itis") %>% 
  mutate(acceptedNameUsage = get_names(acceptedNameUsageID)) %>% 
  select(scientificName, taxonomicStatus, acceptedNameUsage, acceptedNameUsageID)

elton_mam[elton_mam$Scientific == "Spermophilus saturatus",]
elton_mam[elton_mam$Scientific == "Spermophilus saturatus",2] <- need_filter_el$acceptedNameUsageID[1]
elton_mam[elton_mam$Scientific == "Spermophilus saturatus",3] <- need_filter_el$acceptedNameUsage[1]
elton_mam[elton_mam$Scientific == "Spermophilus saturatus",]

elton_mam <- elton_mam  %>% mutate(id_wd = get_ids(Scientific, 'wd',version = 2019)) %>% 
                             mutate(accepted_name_wd = get_names(id_wd, 'wd',version = 2019))

## panTheria trait database
head(panth_data)

panth_mam <- panth_data %>% select(Scientific=MSW05_Binomial) %>% mutate(id = get_ids(Scientific, "itis")) %>% 
                            mutate(accepted_name = get_names(id, "itis"))

panth_mam[panth_mam$Scientific == "Spermophilus saturatus",]
panth_mam[panth_mam$Scientific == "Spermophilus saturatus",2] <- need_filter_el$acceptedNameUsageID[1]
panth_mam[panth_mam$Scientific == "Spermophilus saturatus",3] <- need_filter_el$acceptedNameUsage[1]
panth_mam[panth_mam$Scientific == "Spermophilus saturatus",]

panth_mam <- panth_mam  %>% mutate(id_wd = get_ids(Scientific, 'wd',version = 2019)) %>% 
  mutate(accepted_name_wd = get_names(id_wd, 'wd',version = 2019))

mam$id[which(mam$Scientific == "Spermophilus saturatus")] <- need_filter$acceptedNameUsageID[1]

mam <- mam %>% mutate(accepted_name = get_names(id, "itis"))

length(which(mam$id %in% All_IUCN_names$id == F))
length(which(is.na(mam$id)))

########################################
### end of adding anchor ID for All  ###
########################################

spp_df_all <- read.csv("result/occ_dataframe_taxon_fixed.csv") 

all_cam_spp <- data.frame(Scientific=unique(spp_df_all$speciesScientificName))

all_cam_spp <- all_cam_spp %>% mutate(id = get_ids(Scientific, "itis")) %>% 
                               mutate(accepted_name = get_names(id, "itis"))

all_cam_spp <- all_cam_spp %>% mutate(id_wd = get_ids(Scientific, "wd",version=2019)) %>% 
                               mutate(accepted_name_wd = get_names(id_wd, "wd",version=2019))

head(all_cam_spp)


############################
### See if anchor works  ###
############################


head(all_cam_spp)
head(All_IUCN_names)

head(panth_mam)
head(elton_mam)

nrow(panth_mam)
nrow(panth_data)
head(panth_mam, panth_data)

panth_data <- cbind(panth_data,panth_mam[,2:5])
elton_trait <- cbind(elton_trait,elton_mam[,2:5])

head(all_cam_spp)
names(all_cam_spp)[1] <- "speciesScientificName"
head(spp_df_all)

head(elton_trait)

# left join twice 
head(all_cam_spp)
head(elton_trait)
# 
the_df <- all_cam_spp %>% left_join(elton_trait[,1:7],by="id")
head(the_df)
elton_trait_wd <- elton_trait %>% select(Scientific, ForStrat.Value, diet.breadth, BodyMass.Value, Activity.Nocturnal,
                                        Activity.Diurnal,id_wd, accepted_name_wd)
# Which column does not have a match
the_df_na <- the_df %>% filter(is.na(ForStrat.Value)) %>% select(1:5)

the_df_na
head(elton_trait_wd)
the_df_na$id_wd %in% elton_trait_wd$id_wd  

the_df_na <- the_df_na %>% left_join(elton_trait_wd,by="id_wd")
the_df <- the_df %>% left_join(elton_trait_wd,by="id_wd")


# The IUCN match 

head(All_IUCN_names)
head(elton_trait[,1:7])

the_IUCN_df <- All_IUCN_names %>% left_join(elton_trait[,1:8],by="accepted_name")
head(the_IUCN_df)
the_IUCN_df_na <- the_IUCN_df %>% filter(is.na(ForStrat.Value)) %>% select(1:5)


#### Phase one missing trait 
names <- data.frame(Scientific=unique(unique(missing_trait$speciesScientificName)))
names <- names %>% mutate(id = get_ids(Scientific, "itis")) %>% 
  mutate(accepted_name = get_names(id, "itis"))

names <- names %>% mutate(id_wd = get_ids(Scientific, "wd",version=2019)) %>% 
  mutate(accepted_name_wd = get_names(id_wd, "wd",version=2019))


# Replace NA with -99 and -999

names$id[is.na(names$id)] <- -99
names$accepted_name[is.na(names$accepted_name)] <- -99
names$id_wd[is.na(names$id_wd)] <- -99
names$accepted_name_wd[is.na(names$accepted_name_wd)] <- -99

# Replace Elton NA with -999

elton_mam$id[is.na(elton_mam$id)] <- -999
elton_mam$accepted_name[is.na(elton_mam$accepted_name)] <- -999
elton_mam$id_wd[is.na(elton_mam$id_wd)] <- -999
elton_mam$accepted_name_wd[is.na(elton_mam$accepted_name_wd)] <- -999

head(elton_mam)


length(names$id)
length(which(names$id %in% elton_mam$id))
length(which(names$accepted_name %in% elton_mam$accepted_name))


ind <- which(names$id %in% elton_mam$id)
names_2nd <- names[-ind,]

length(which(names_2nd$id_wd %in% elton_mam$id_wd))
which(names_2nd$id_wd %in% elton_mam$id_wd)

name_3rd <- names_2nd

head(name_3rd) 

# search for IUCN synonyms and see if it's in the trait database

for (i in 1:nrow(name_3rd)) {
 IUCN_result <- rredlist::rl_synonyms(name_3rd$Scientific[i],key="29c88e9e867726644b28997693189b9a9301b21e4bdf0280d51b3006bebc6642")
 IUCN_syn <-  IUCN_result$result$synonym[1]
 if(IUCN_result$count != 0){
 name_3rd$IUCN_syn[i] <- IUCN_syn
 } else {
   name_3rd$IUCN_syn[i] <- "No_IUCN_synonym" 
  }
 }

name_3rd$IUCN_syn_in_trait_data <- name_3rd$IUCN_syn %in% elton_mam$Scientific
length(which(name_3rd$IUCN_syn_in_trait_data == T) )

name_3rd$accepted_name[ (name_3rd$IUCN_syn_in_trait_data == T) ] <- name_3rd$IUCN_syn[ (name_3rd$IUCN_syn_in_trait_data == T) ] 

# The final need look up species
check_one_by_one <-  name_3rd %>% filter(IUCN_syn_in_trait_data == F)

check_one_by_one$Scientific

# Remove one wrong row

name_3rd <- name_3rd[-which(name_3rd$Scientific == "All_in"),]
check_one_by_one <- check_one_by_one[-which(check_one_by_one$Scientific == "All_in"),]
check_one_by_one$Scientific
#############################################################################
##  Use accepted_name as an anchor column to joint species （find trait）###
#############################################################################


#[1] "Aotus zonalis"          "Bassaricyon medius"     "Alouatta discolor"      "Pithecia chrysocephala"  "Bos gaurus" "Piliocolobus oustaleti"
#[7] "Eulemur rufifrons"      "Avahi peyrierasi"       "Cephalophus harveyi"    "Acomys ngurui"

# "Aotus zonalis" == "Aotus lemurinus"
elton_mam[which(elton_mam$Scientific == "Aotus lemurinus"),]

#"Bassaricyon medius" == "Bassaricyon alleni"
elton_mam[which(elton_mam$Scientific == "Bassaricyon alleni"),]

# "Alouatta discolor" == "Alouatta belzebul"
elton_mam[which(elton_mam$Scientific == "Alouatta belzebul"),]

# "Pithecia chrysocephala" == "Pithecia pithecia"
elton_mam[which(elton_mam$Scientific == "Pithecia pithecia"),]

# "Bos gaurus" == "Bos frontalis"
elton_mam[which(elton_mam$Scientific == "Bos frontalis"),]

# "Piliocolobus oustaleti"
elton_mam[which(elton_mam$Scientific == "Piliocolobus rufomitratus"),]

# "Eulemur rufifrons"
elton_mam[which(elton_mam$Scientific == "Eulemur rufus"),]

# "Avahi peyrierasi"  == "Avahi laniger"
elton_mam[which(elton_mam$Scientific == "Avahi laniger"),]

# "Cephalophus harveyi" == "Cephalophus natalensis"
elton_mam[which(elton_mam$Scientific == "Cephalophus natalensis"),]

# "Acomys ngurui" == "Acomys spinosissimus"

elton_mam[which(elton_mam$Scientific == "Acomys spinosissimus"),]


#[1] "Muntiacus montanus"         "Neofelis diardi"             "Herpailurus yagouaroundi"   "Canis lupaster"           
#[6] "Sylvilagus gabbi"            "Herpestes sanguineus"       "Hylomyscus vulcanorum"      "Phataginus tetradactyla"             
#[11] "Mazama nemorivaga"          "Toromys grandis"            "Dasyprocta croconota"       "Cheirogaleus grovesi"      
#[16] "Plecturocebus urubambensis" "Leontocebus weddelli"       "Dasyprocta variegata"       "Hylomyscus arcimontensis"      
#[21]  "Biswamoyopterus laoensis"   "Philander mondolfii"       "Hylomyscus walterverheyeni"  "Maxomys tajuddinii"
#[26]  "Procolobus gordonorum"      "Rhynchocyon udzungwensis"   "Saimiri cassiquiarensis"    "Marmosa alstoni"                 
#[31]   "Pattonomys occasius"                

# "Muntiacus montanus" == "Muntiacus muntjak"
elton_mam[which(elton_mam$Scientific == "Muntiacus muntjak"),]

#"Neofelis diardi" =="Neofelis nebulosa"
elton_mam[which(elton_mam$Scientific == "Neofelis nebulosa"),]

# "Herpailurus yagouaroundi" == "Puma yagouaroundi"
elton_mam[which(elton_mam$Scientific == "Puma yagouaroundi"),]


# "Canis lupaster" == "Canis lupus"  
elton_mam[which(elton_mam$Scientific == "Canis lupus"),]

# "Sylvilagus gabbi"   == "Sylvilagus brasiliensis"
elton_mam[which(elton_mam$Scientific == "Sylvilagus brasiliensis"),]

# "Herpestes sanguineus"  ==  "Galerella sanguinea"
elton_mam[which(elton_mam$Scientific == "Galerella sanguinea"),]

# "Hylomyscus vulcanorum" == "Hylomyscus denniae"
elton_mam[which(elton_mam$Scientific == "Hylomyscus denniae"),]

# "Phataginus tetradactyla" == "Manis tetradactyla"
elton_mam[which(elton_mam$Scientific == "Manis tetradactyla"),]

# "Mazama nemorivaga"  == "Mazama gouazoubira"
elton_mam[which(elton_mam$Scientific == "Mazama gouazoubira"),]

# "Toromys grandis" == "Makalata grandis"
elton_mam[which(elton_mam$Scientific == "Makalata grandis"),]

# "Dasyprocta croconota" == "Dasyprocta leporina"
elton_mam[which(elton_mam$Scientific == "Dasyprocta leporina"),]

# "Cheirogaleus grovesi"  == "Cheirogaleus crossleyi"
elton_mam[which(elton_mam$Scientific == "Cheirogaleus crossleyi"),]

# "Plecturocebus urubambensis" == "Callicebus brunneus"
elton_mam[which(elton_mam$Scientific == "Callicebus brunneus"),]

# "Leontocebus weddelli" == "Saguinus nigricollis"
elton_mam[which(elton_mam$Scientific == "Saguinus nigricollis"),]

# "Dasyprocta variegata" == "Dasyprocta leporina"
elton_mam[which(elton_mam$Scientific == "Dasyprocta leporina"),]

# "Hylomyscus arcimontensis"  ==  "Hylomyscus denniae"
elton_mam[which(elton_mam$Scientific == "Hylomyscus denniae"),]

# "Biswamoyopterus laoensis" == "Biswamoyopterus biswasi"
elton_mam[which(elton_mam$Scientific == "Biswamoyopterus biswasi"),]

# "Philander mondolfii" == "Philander opossum"
elton_mam[which(elton_mam$Scientific == "Philander opossum"),]

# "Hylomyscus walterverheyeni" == "Hylomyscus alleni"
elton_mam[which(elton_mam$Scientific == "Hylomyscus alleni"),]

# "Maxomys tajuddinii" == "Maxomys whiteheadi"
elton_mam[which(elton_mam$Scientific == "Maxomys whiteheadi"),]

# "Procolobus gordonorum" == "Piliocolobus gordonorum"
elton_mam[which(elton_mam$Scientific == "Piliocolobus gordonorum"),]

# "Rhynchocyon udzungwensis" == "Rhynchocyon cirnei"
elton_mam[which(elton_mam$Scientific == "Rhynchocyon cirnei"),]

# "Saimiri cassiquiarensis" == "Saimiri sciureus"
elton_mam[which(elton_mam$Scientific == "Saimiri sciureus"),]

#  "Marmosa alstoni" == "Micoureus alstoni"
elton_mam[which(elton_mam$Scientific == "Micoureus alstoni"),]


# Previously checked left over
# [1] "Galeopterus variegatus" "Monodelphis ronaldi"    "Muntiacus vaginalis"    "Microgale majori"       "Tragelaphus oryx"       "Pithecia inusta"       
# [7] "Pattonomys occasius"   

# "Pattonomys occasius" == "Makalata occasius"
elton_mam[which(elton_mam$Scientific == "Makalata occasius"),]


Scientific_names  <- c("Aotus zonalis","Bassaricyon medius", "Alouatta discolor",  "Pithecia chrysocephala" , "Bos gaurus", "Piliocolobus oustaleti",
"Eulemur rufifrons","Avahi peyrierasi", "Cephalophus harveyi",  "Acomys ngurui", "Muntiacus montanus",       
"Neofelis diardi" ,  "Herpailurus yagouaroundi","Canis lupaster",          
 "Sylvilagus gabbi",  "Herpestes sanguineus" ,  "Hylomyscus vulcanorum", "Phataginus tetradactyla",             
"Mazama nemorivaga",  "Toromys grandis",  "Dasyprocta croconota" ,      "Cheirogaleus grovesi" ,     
"Plecturocebus urubambensis", "Leontocebus weddelli" , "Dasyprocta variegata" , "Hylomyscus arcimontensis",      
"Biswamoyopterus laoensis",  "Philander mondolfii",  "Hylomyscus walterverheyeni" , "Maxomys tajuddinii",
"Procolobus gordonorum", "Rhynchocyon udzungwensis" , "Saimiri cassiquiarensis", "Marmosa alstoni","Galeopterus variegatus", 
"Monodelphis ronaldi","Muntiacus vaginalis", "Microgale majori","Tragelaphus oryx", "Pithecia inusta",  "Pattonomys occasius")         


anchor <- c("Aotus lemurinus","Bassaricyon alleni","Alouatta belzebul","Pithecia pithecia","Bos frontalis","Piliocolobus rufomitratus",
"Eulemur rufus" ,"Avahi laniger","Cephalophus natalensis","Acomys spinosissimus","Muntiacus muntjak",
"Neofelis nebulosa","Puma yagouaroundi","Canis lupus" ,
"Sylvilagus brasiliensis", "Galerella sanguinea", "Hylomyscus denniae","Manis tetradactyla", 
"Mazama gouazoubira","Makalata grandis","Dasyprocta leporina", "Cheirogaleus crossleyi","Callicebus brunneus", "Saguinus nigricollis",
"Dasyprocta leporina", "Hylomyscus denniae","Biswamoyopterus biswasi","Philander opossum","Hylomyscus alleni","Maxomys whiteheadi",
"Piliocolobus gordonorum","Rhynchocyon cirnei", "Saimiri sciureus", "Micoureus alstoni","Galeopterus variegates",
"Monodelphis adusta","Muntiacus muntjak","Microgale longicaudata","Taurotragus derbianus","Pithecia pithecia","Makalata occasius")

length(Scientific_names)
length(anchor)

anchor_names <- data.frame(Scientific=Scientific_names,anchor=anchor )

anchor_names

write.csv(anchor_names,"result/anchor_names_for_spps_found_no_traits.csv",row.names = F)

anchor_names <- read.csv("result/anchor_names_for_spps_found_no_traits.csv")




