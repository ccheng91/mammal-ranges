# Add covariates
library(dplyr)

# load df
modelling_df <- read.csv("result/modeling_df.csv")

# add species traits
# Eltonian traits

fun_data <- read.delim("data/MamFuncDat.txt")
Panth_trait_data <- read.delim("data/ECOL_90_184/PanTHERIA_1-0_WR05_Aug2008.txt")

head(fun_data)
str(trait_data)
# 1. body mass. 2. Foraging stratum 4. nocturnal 5. diet breadth. 6 Group size

# diet breadth
diet_matrix <- fun_data[,4:13] 
diet.breadth <- rowSums(diet_matrix != 0)
fun_data$diet.breadth <- diet.breadth

elton_trait <- fun_data %>% dplyr::select(Scientific,ForStrat.Value,diet.breadth,BodyMass.Value, 
                                Activity.Nocturnal,Activity.Diurnal)


# large group sized 
trait_data <- Panth_trait_data %>%  dplyr::select(Scientific=MSW05_Binomial,X15.1_LitterSize,
                                            X10.1_PopulationGrpSize,X10.2_SocialGrpSize)

# No-matches of two trait base
length(which(trait_data$MSW05_Binomial %in% elton_trait$Scientific == F))

speices_trait <- left_join(elton_trait,trait_data,by="Scientific")
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
modelling_df_add_trait <- left_join(modelling_df,speices_trait,by="speciesScientificName")

## try 
missing_trait <- modelling_df_add_trait[is.na(modelling_df_add_trait$ForStrat.Value),]

## Find if it mismatches has Synomyms and if Synomyms is in trait database
head(recent_spp)
head(modelling_df_add_trait)

missing_trait$has_synomys <- 9999
missing_trait$synomys_in_trait <- 9999
missing_trait$synomys_name <- 9999

for (i in 1:nrow(missing_trait)) {
  syn <- rredlist::rl_synonyms(missing_trait$speciesScientificName[i], key="29c88e9e867726644b28997693189b9a9301b21e4bdf0280d51b3006bebc6642")
  
  if(syn$count == 0){
  missing_trait$has_synomys[i] <- "Nope"
  missing_trait$synomys_in_trait[i] <- "Nope"
  missing_trait$synomys_name[i] <- "Nope"
  } else if(syn$count == 1) {
    
    missing_trait$has_synomys[i] <- T 
    In_or_Not <- syn$result$synonym %in% speices_trait$speciesScientificName 
    missing_trait$synomys_in_trait[i] <- In_or_Not
   if(In_or_Not){
     missing_trait$synomys_name[i] <- syn$result$synonym
    }else{
      missing_trait$synomys_name[i] <- "Has_synomys_but_not_in_trait_base"
    }
    
  } else if(syn$count > 1){
    
    In_or_Not <- syn$result$synonym %in% speices_trait$speciesScientificName
    
    if (any(In_or_Not == T)) {
      
      missing_trait$has_synomys[i] <- T
      missing_trait$synomys_in_trait[i] <- T
      missing_trait$synomys_name[i] <- syn$result$synonym[In_or_Not][1]
      
    } else{
      missing_trait$has_synomys[i] <- T
      missing_trait$synomys_in_trait[i] <- F
      missing_trait$synomys_name[i] <- "Has_synomys_but_not_in_trait_base"
    }
    }
}


length(which(missing_trait$has_synomys == "Nope"))

which(speices_trait$speciesScientificName == "Catopuma temminckii" )

"Muntiacus montanus"


# Location covariate

unique(missing_trait$speciesScientificName)

