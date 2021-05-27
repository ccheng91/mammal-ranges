# Add covariates 2
library(dplyr)
#modelling_df <- read.csv("result/2.0-modelling_df_with_traits.csv")
modelling_df <- read.csv("result/2.0-modelling_df_with_traits_emml_etc_added.csv")

modelling_df$speciesScientificName

# Add IUCN history 
his <- rredlist::rl_history(modelling_df$speciesScientificName[1],key="29c88e9e867726644b28997693189b9a9301b21e4bdf0280d51b3006bebc6642")

#
spp <- unique(modelling_df$speciesScientificName)

iucn_his <- data.frame(name=as.character(),frq=as.numeric(),year=as.numeric())


for(i in 1:length(spp)){
  his <- rredlist::rl_history(spp[i],key="29c88e9e867726644b28997693189b9a9301b21e4bdf0280d51b3006bebc6642")
  
  if(rlang::is_empty(his$result)){
    cc <- data.frame( name=spp[i],frq="NA_not_in_iucn",year="NA_not_in_iucn")
  } else{
    cc <- data.frame( name=spp[i],frq=nrow(his$result),year=his$result$year[1])
  }
  iucn_his <- rbind(iucn_his,cc)
}

iucn_his[-which(iucn_his$frq =="NA_not_in_iucn"),]

write.csv(iucn_his, "result/2.0-the_IUCN_history.csv", row.names = F)

names(iucn_his) <- c("speciesScientificName","IUCN_frq","IUCN_year")
head(modelling_df)

modelling_df <- left_join(modelling_df,iucn_his,by="speciesScientificName") 
modelling_df <- modelling_df %>%  filter(IUCN_frq != "NA_not_in_iucn") 

#  Camelus dromedarius - domestic removed
#  Tamiops macclellandii small squirrel removed


## Add site covs
pre_df <- read.csv("data/df/Chen_etal_Dataframe_for_model_Final_ori-scale.csv") 
sampling_eff <- read.csv("data/df/3.1-Totall_sampling_effort_JUL2020.csv") %>% dplyr::select(projectID, sampling_effort_t)


#dplyr::select(projectID,elevation,Tree_mean,realm )

head(pre_df)
pre_df <-pre_df %>% left_join(sampling_eff,by="projectID") %>% 
  dplyr::select(projectID,realm, elevation,NPP,ave_temp,Tree_mean,sampling_effort_t) 

df <- modelling_df  %>% left_join(pre_df,by="projectID" )

df[which(df$ForStrat.Value == "M"),]
df <- df[-which(df$ForStrat.Value == "M"),] # remove marine spp

write.csv(df,"result/2.5-modelling_df_all_covs.csv",row.names = F)



