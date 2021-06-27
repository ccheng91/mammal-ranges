## Modelling
library(dplyr)
df <- read.csv("result/2.5-modelling_df_all_covs.csv")

head(df)
df$diet.breadth <- scale(df$diet.breadth)

df$BodyMass.Value <- scale(df$BodyMass.Value)

df$elevation <- scale(df$elevation)

df$NPP <- scale(df$NPP)

df$ave_temp <- scale(df$ave_temp)

df$Tree_mean <- scale(df$Tree_mean)

df$IUCN_year <- scale(as.numeric(df$IUCN_year))

df$IUCN_frq <- as.numeric(df$IUCN_frq)

df$sampling_effort_t <- scale(df$sampling_effort_t)


df <- df[-which(is.na(df$elevation)==T),]
df <- df[-which(is.na(df$Activity.Nocturnal)==T),]
table(df$ForStrat.Value)
table(df$realm)
table(df$Activity.Nocturnal)
table(df$diet.breadth)
table(df$IUCN_year)

min(df$Tree_mean,na.rm = T)
max(df$Tree_mean,na.rm = T)


min(df$elevation,na.rm = T)
max(df$elevation,na.rm = T)
## Break 

omi <- df %>% filter(type == "A" | type == "B")
com <- df %>% filter(type == "C" | type == "B")


omi$type[which(omi$type == "A")] <- 1
com$type[which(com$type == "C")] <- 1

omi$type[which(omi$type == "B")] <- 0
com$type[which(com$type == "B")] <- 0

omi$type <- as.numeric(omi$type)
com$type <- as.numeric(com$type)

head(df)
head(omi)
library(lme4)
omi <- omi %>% dplyr::select(type, speciesScientificName ,ForStrat.Value , Activity.Nocturnal , diet.breadth, BodyMass.Value , realm , 
                               Tree_mean , elevation , IUCN_frq , IUCN_year , sampling_effort_t)

z <- glm(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value + realm + 
           Tree_mean + elevation + IUCN_frq + IUCN_year + sampling_effort_t, family=binomial(),data=omi)

summary(z)

simulationOutput1 <- DHARMa::simulateResiduals(fittedModel = z, plot = F)
plot(simulationOutput1)


names(omi)

fm1 <- glmer(type ~  ForStrat.Value + Activity.Nocturnal , diet.breadth, BodyMass.Value, realm, 
             Tree_mean , elevation , IUCN_frq , IUCN_year , sampling_effort_t + (1|speciesScientificName),data = omi,family = binomial,control = glmerControl(optimizer = "bobyqa"))


dd <- MuMIn::dredge(fm1)


options(na.action = "na.omit")


z1 <- glm(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value + realm + Tree_mean + elevation,family=binomial(),data=com)

summary(z1)

length(unique(df$speciesScientificName))



gz <- glmer(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value 
            + realm + Tree_mean + elevation + (1 | projectID ), data = omi , family = binomial)

summary(gz)


gz1 <- glmer(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value +
               IUCN_frq + IUCN_year + sampling_effort_t +
              realm + Tree_mean + elevation + (1 | speciesScientificName ), data = omi , 
             family = binomial,control = glmerControl(optimizer = "bobyqa"))

options(na.action = "na.fail")
dd <- MuMIn::dredge(gz1)
  


options(na.action = "na.omit")

summary(gz1)
print(gz1)


se <- sqrt(diag(vcov(gz1)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(gz1), LL = fixef(gz1) - 1.96 * se, UL = fixef(gz1) + 1.96 *
                se))

print(gz2)
summary(gz2)

gz5 <- glmer(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value 
             + realm + Tree_mean + elevation + (1| speciesScientificName ) + (1| projectID), data = com , 
             family = binomial,control = glmerControl(optimizer = "bobyqa"))

print(gz5)
summary(gz5)


m0.glm <-   glm(type ~ 1, family = binomial, data=com)
m0.glmer <-  glmer(type ~ (1| speciesScientificName ), data = com, 
                   family = binomial)

aic.glmer <- AIC(logLik(m0.glmer))
aic.glm <- AIC(logLik(m0.glm))
aic.glmer; aic.glm

null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
pchisq(as.numeric(null.id), df=1, lower.tail=F) 
