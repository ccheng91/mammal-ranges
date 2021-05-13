# Add covariates 2
library(dplyr)
df <-modelling_df_final

terr_mal_new

rredlist::rl_history(terr_mal_new$binomial[1],)

head(df)

pre_df <- read.csv("data/df/5.0-dataframe_for_modelling_OCT2019.csv") %>% dplyr::select(projectID,elevation,Tree_mean,realm )

df <- df %>% left_join(pre_df,by="projectID" )
 
head(pre_df)

omi <- df %>% filter(type == "A" | type == "B")

com <- df %>% filter(type == "C" | type == "B")
  
table(df$realm)

omi$type[which(omi$type == "A")] <- 1
com$type[which(com$type == "C")] <- 1

omi$type[which(omi$type == "B")] <- 0
com$type[which(com$type == "B")] <- 0

omi$type <- as.numeric(omi$type)
com$type <- as.numeric(com$type)


z <- glm(type ~ diet.breadth+ BodyMass.Value + realm,family=binomial(),data=omi)

summary(z)


z1 <- glm(type ~ diet.breadth+ BodyMass.Value,family=binomial(),data=com)

summary(z1)


