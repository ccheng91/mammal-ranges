## Modelling
library(dplyr)
library(lme4)

rm(list=ls(all=TRUE))
df <- read.csv("result/June2021/2.5-modelling_df_all_covs.csv") %>% unique()

# filter again (species < 500)
df <- df[-which(df$BodyMass.Value <=500),]

head(df)
df$diet.breadth <- scale(df$diet.breadth)

df$BodyMass.Value <- scale(df$BodyMass.Value)

df$elevation <- scale(df$elevation)

df$NPP <- scale(df$NPP)

df$ave_temp <- scale(df$ave_temp)

df$Tree_mean <- scale(df$Tree_mean)

df$IUCN_year <- scale(as.numeric(df$IUCN_year))

df$IUCN_frq <-  scale(as.numeric(df$IUCN_frq))

df$sampling_effort_t <- scale(df$sampling_effort_t)

df$ForStrat.Value <- relevel(as.factor(df$ForStrat.Value), ref = "G" )

class(df$ForStrat.Value)

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

##############################################
# Need descriptive analysis about omission ##
#############################################
type_a <-  df %>% filter(type == "A") 
table(type_a$speciesScientificName)
length(unique(type_a$speciesScientificName))

library(sf)
TERR_mal <- st_read("data/MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
shap <- st_read("data/all_in_one_folder_OCT2019/All_area_shape_NOV2020.shp")

EMML_DYTP <- shap[shap$projectID == "EMML_DYTP",]
Herpestes_urva <- TERR_mal[TERR_mal$binomial  == "Herpestes urva",]

croped <- st_intersects(Herpestes_urva, EMML_DYTP, sparse = FALSE)


IUCN_spp <- unique(Herpestes_urva$binomial[croped])

plot(Herpestes_urva)

plot(Herpestes_urva[3,])

type_a[type_a$speciesScientificName =="Herpestes urva",]


type_a[type_a$speciesScientificName =="Sus scrofa",]
##############################################
# Need descriptive analysis about omission ##
#############################################

omi$type[which(omi$type == "A")] <- 1
com$type[which(com$type == "C")] <- 1

omi$type[which(omi$type == "B")] <- 0
com$type[which(com$type == "B")] <- 0

omi$type <- as.numeric(omi$type)
com$type <- as.numeric(com$type)

length(which(df$type=="A"))
length(which(df$type=="B"))
length(which(df$type=="C"))

# prepare the table
tab <- as.data.frame.matrix(table(df$speciesScientificName , df$projectID))
tab$total <- rowSums(tab)

## instric exstric
z.n <- glmer(type ~ 1+ 
               (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = omi )

z.f <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
               IUCN_frq + IUCN_year +  realm + sampling_effort_t +
               Tree_mean +
               (1|speciesScientificName) + (1|projectID), family = binomial, glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), data = omi )
# speices
z1 <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = omi )
# samplling
z2 <- glmer(type ~ IUCN_frq + IUCN_year +  realm + sampling_effort_t + 
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)), data = omi )
# habitat
z3 <- glmer(type ~ Tree_mean +
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = omi )

# speices + samplling
z4 <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
              IUCN_frq + IUCN_year +  realm + sampling_effort_t + 
              (1|speciesScientificName) + (1|projectID), family = binomial, glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')), data = omi )

# speices + habitat
z5 <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
              Tree_mean +
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = omi )

#samplling + habitat
z6 <- glmer(type ~ IUCN_frq + IUCN_year +  realm + sampling_effort_t + 
              Tree_mean +
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = omi )

# for commission error 

x.n <- glmer(type ~ 1+ 
               (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

x.f <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
               IUCN_frq + IUCN_year +  realm + sampling_effort_t +
                Tree_mean +
               (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

x1 <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

x2 <- glmer(type ~ IUCN_frq + IUCN_year +  realm + sampling_effort_t + IUCN_cat+
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

x3 <- glmer(type ~ Tree_mean +
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )


# speices + samplling
x4 <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
              IUCN_frq + IUCN_year +  realm + sampling_effort_t + 
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

# speices + habitat
x5 <- glmer(type ~ BodyMass.Value + ForStrat.Value + diet.breadth  + Activity.Nocturnal + 
              Tree_mean +
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

#samplling + habitat
x6 <- glmer(type ~ IUCN_frq + IUCN_year +  realm + sampling_effort_t + 
              Tree_mean +
              (1|speciesScientificName) + (1|projectID), family = binomial, control=glmerControl(optimizer="bobyqa"), data = com )

####################
## Model selection #
####################

cant.st1 <- list(z.n,z.f,z1,z2,z3,z4,z5,z6)

modnames1 <-  c("Null","Full","SpeicesTraits","Sampling","Habitat","SpeicesTraits + Sampling","SpeicesTraits + Habitat",
                "Sampling + Habitat")

modsel1 <- AICcmodavg::aictab(cant.st1,modnames1)


cant.st2 <- list(x.n,x.f,x1,x2,x3,x4,x5,x6)

modnames2 <-  c("Null","Full","SpeicesTraits","Sampling","Habitat","SpeicesTraits + Sampling","SpeicesTraits + Habitat",
                "Sampling + Habitat")

modsel2 <- AICcmodavg::aictab(cant.st2,modnames2)

write.csv(modsel1,"result/June2021/3.0-modelsel_omission.csv",row.names = F)
write.csv(modsel2,"result/June2021/3.0-modelsel_commission.csv",row.names = F)


####################
## cater plot ######
####################

library(ggplot2)

summary(x.f)
       
estimate <- fixef(x.f)
names(estimate)
as.data.frame(estimate)

data.frame(name=names(estimate), estimate=unname(estimate))

confint.merMod(x.f, method = "Wald")
fff <- as.data.frame(t(confint.merMod(x.f , method = "Wald")))

pr <- ggeffects::ggpredict(x.f, c("ForStrat.Value"), type = "fixed")

plot(pr)

pr2 <- ggeffects::ggpredict(x.f, c("IUCN_frq"), type = "fixed")



## set x-axis to original scale

unscale(pr2$x)
df.ori <- read.csv("result/June2021/2.5-modelling_df_all_covs.csv") %>% unique()
IUCN_frq <- scale(as.numeric(df.ori$IUCN_frq))
attr(IUCN_frq,'scaled:scale')

pr2$x <- pr2$x * attr(IUCN_frq, 'scaled:scale') + attr(IUCN_frq, 'scaled:center')

library(ggplot2)
plot(pr2) +labs(
  x = "IUCN assessment frequency (times)", 
  y = "Probabilities", 
  title = "Predicted probabilities of commission error")

max(df.ori$IUCN_frq)

#ITBD <- df[df$projectID=='ITBD',]
#ITBD_moose <- df[df$speciesScientificName=='Alces alces',]
#
#head(ITBD_moose)
#
#head(df)
#head(omi)
#
#omi <- omi %>% dplyr::select(type, speciesScientificName ,ForStrat.Value , Activity.Nocturnal , diet.breadth, BodyMass.Value , realm , 
#                               Tree_mean , elevation , IUCN_frq , IUCN_year , sampling_effort_t)
#
#z <- glm(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value + realm + 
#           Tree_mean + elevation + IUCN_frq + IUCN_year + sampling_effort_t, family=binomial(),data=omi)
#
#summary(z)
#
#simulationOutput1 <- DHARMa::simulateResiduals(fittedModel = z, plot = F)
#plot(simulationOutput1)
#
#
#names(omi)
#
#fm1 <- glmer(type ~  ForStrat.Value + Activity.Nocturnal , diet.breadth, BodyMass.Value, realm, 
#             Tree_mean , elevation , IUCN_frq , IUCN_year , sampling_effort_t + (1|speciesScientificName),data = omi,family = binomial,control = glmerControl(optimizer = "bobyqa"))
#
#
#dd <- MuMIn::dredge(fm1)
#
#
#options(na.action = "na.omit")
#
#
#z1 <- glm(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value + realm + Tree_mean + elevation,family=binomial(),data=com)
#
#summary(z1)
#
#length(unique(df$speciesScientificName))
#
#
#
#gz <- glmer(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value 
#            + realm + Tree_mean + elevation + (1 | projectID ), data = omi , family = binomial)
#
#summary(gz)
#
#
#gz1 <- glmer(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value +
#               IUCN_frq + IUCN_year + sampling_effort_t +
#              realm + Tree_mean + elevation + (1 | speciesScientificName ), data = omi , 
#             family = binomial,control = glmerControl(optimizer = "bobyqa"))
#
#options(na.action = "na.fail")
#dd <- MuMIn::dredge(gz1)
#  
#
#
#options(na.action = "na.omit")
#
#summary(gz1)
#print(gz1)
#
#
#se <- sqrt(diag(vcov(gz1)))
## table of estimates with 95% CI
#(tab <- cbind(Est = fixef(gz1), LL = fixef(gz1) - 1.96 * se, UL = fixef(gz1) + 1.96 *
#                se))
#
#print(gz2)
#summary(gz2)
#
#gz5 <- glmer(type ~ ForStrat.Value + Activity.Nocturnal + diet.breadth+ BodyMass.Value 
#             + realm + Tree_mean + elevation + (1| speciesScientificName ) + (1| projectID), data = com , 
#             family = binomial,control = glmerControl(optimizer = "bobyqa"))
#
#print(gz5)
#summary(gz5)
#
#
#m0.glm <-   glm(type ~ 1, family = binomial, data=com)
#m0.glmer <-  glmer(type ~ (1| speciesScientificName ), data = com, 
#                   family = binomial)
#
#aic.glmer <- AIC(logLik(m0.glmer))
#aic.glm <- AIC(logLik(m0.glm))
#aic.glmer; aic.glm
#
#null.id = -2 * logLik(m0.glm) + 2 * logLik(m0.glmer)
#pchisq(as.numeric(null.id), df=1, lower.tail=F) 
#