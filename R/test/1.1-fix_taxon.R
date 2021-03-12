
# Name to be fixed 
      
 "Cebus capucinus"          "Cabassous centralis"        "Cercopithecus lhoesti"   
 "Galagoides demidovii"                  "Cabassous unicinctus"    
 "Priodontes maximus"       "Cebus apella"                             
                            "Procolobus gordonorum"   
 "Pekania pennanti"                         "Madoqua Kirkii"          
 "Ardeotis kori"            "Taurotragus oryx"         "Lepus microtis"          
 "Sagittarius serpentarius" "Damaliscus korrigum"      "Martes Pennanti"         
 "Erethizion dorsatum"      "Tamias dorsalis"          "Canis aureus"            
 "Camelus dromedarius"      

# Fix
 
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
  filter( speciesScientificName !=  "Peromyscus sp.") 
  
write.csv(spp_df_all,"result/occ_dataframe_taxon_fixed.csv",row.names = F)

