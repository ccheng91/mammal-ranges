library(dplyr)
library(rgbif)
library(lubridate)

rm(list=ls(all=TRUE))

## make occurrence record data
spp_path <- list.files("/Users/chencheng/Desktop/data/TEAMS/meta_database/Image/Image_filtered",
                       ignore.case = TRUE, full.names=TRUE, recursive=TRUE)
team_path <- spp_path[4:19]
head(team_path)

# Note:
# image:deploymentID = deployment:deploymentLocationID

dep_team <- read.csv("/Users/chencheng/Desktop/data/TEAMS/meta_database/Deployment/Deployment_TEAMS.csv",
                     stringsAsFactors = F, header = T)
dep_3 <- dep_team %>% dplyr::select(deploymentID=deploymentLocationID,Latitude, Longitude) %>% unique()

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

dep_pro1 <- read.csv("/Users/chencheng/Desktop/data/TEAMS/meta_database/Deployment/Deployment_GC.csv")
dep_pro2 <- read.csv("/Users/chencheng/Desktop/data/TEAMS/meta_database/Deployment/Deployment_ITBD.csv")
dep_pro3 <- read.csv("/Users/chencheng/Desktop/data/TEAMS/meta_database/Deployment/Deployment_SRGT.csv")

dep_others <- rbind(dep_pro1,dep_pro2)

dep_others <- dep_others %>% dplyr::select(deploymentID=deploymentLocationID, Latitude, Longitude) %>% unique()

dep_pro_srgt <- dep_pro3 %>% dplyr::select(deploymentID=deploymentID, Latitude, Longitude) %>% unique()

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


# Emammal 

emmal_path <- spp_path[20:21]

dep_path_emammal_US <- list.files("/Users/chencheng/Desktop/data/TEAMS/meta_database/Emml_redownload_NOV2019/all_deployment", 
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

  emml_US <- read.csv(spp_path[21],stringsAsFactors = F, header = T)
  image_temp <- emml_US %>% dplyr::select(deploymentID,imageSequenceBeginTime,speciesScientificName) %>% 
    mutate(date=as.Date((imageSequenceBeginTime))) %>% mutate(year=year(date)) %>% 
    dplyr::select(deploymentID,speciesScientificName,year) %>% unique()

### Making right deploymentID for emml_dep_US
### Match project name and attach EMML_abbr_ to deployment ID
  
head(deply_emammal_us)
  
site_info_emml <- read.csv("/Users/chencheng/Desktop/data/TEAMS/meta_database/Emml_redownload_NOV2019/project_info_US_redownloaded_NOV2019.csv", stringsAsFactors = F)
  
correct_BE_time <- deply_emammal_us %>% dplyr::select(projectID= project_id, deployment_id,actual_date_out,retrieval_date,project_name,
                                                 sub_project_name,camera_failure_details,quiet_period_setting,actual_lat, actual_long)
  
  head(site_info_emml)
  head(correct_BE_time)
  
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
  
### Emammal other #####
 emml_other_temp <- read.csv(spp_path[20],stringsAsFactors = F, header = T)
 
 dep_emml <- read.csv("/Users/chencheng/Desktop/data/TEAMS/meta_database/Deployment/Deployment_EMML.csv", 
                    stringsAsFactors = F, header = T)
  head(emml_other_temp)
  head(dep_emml)
 
 dep_emml_other <- dep_emml %>% dplyr::select(deploymentID,Latitude, Longitude)
 
 image_temp <- emml_other_temp %>% dplyr::select(deploymentID,imageSequenceBeginTime,speciesScientificName) %>% 
   mutate(date=as.Date((imageSequenceBeginTime))) %>% mutate(year=year(date)) %>% 
   dplyr::select(deploymentID,speciesScientificName,year) %>% unique()

  spp_df4 <- left_join(image_temp ,dep_emml_other, by="deploymentID") %>% unique()
  
## add XSBN data

XSBN <- read.csv("/Users/chencheng/Desktop/data/TEAMS/data/cameratrap/xsbn.csv", stringsAsFactors = F, header = T)
XSBN_effort <- read.csv("/Users/chencheng/Desktop/data/TEAMS/data/XSBN_efforts.csv", stringsAsFactors = F, header = T)
  
head(XSBN)  
head(XSBN_effort)

XSBN_dep <- XSBN_effort %>% dplyr::select(deploymentID=NO, Longitude=GPS_X, Latitude=GPS_Y)

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

write.csv(spp_df_all,"result/occ_dataframe.csv",row.names = F)

# All species
all_spp <- spp_df_all$speciesScientificName 
all_spp <- unique(all_spp)

### 

cam_spp <- all_spp
# check all spp names:

need_check <- cam_spp
need_check <- data.frame(spp_name=need_check,itis_name=9999 ,check=9999,stringsAsFactors = F)
str(need_check)


# change 
a <- list()
for(i in 1:length(all_spp)){ # check spp name in itis database 
  ero <- try( a[[i]] <- checkSpeciesNames(all_spp[i], accepted = F,searchtype="scientific", ask=F), TRUE)
  if(isTRUE(class(ero)=="try-error")) { # ignore the error and keep looping, return the original name if error 
    a[[i]] <- all_spp[i]
  } 
}

for (i in 1:length(a)) {
  if(length(a[[i]]) != 1) {
    need_check$itis_name[i] <- a[[i]]$scientificName
    need_check$check[i] <- a[[i]]$taxonUsageRating
  } else{
    need_check$itis_name[i] <- a[[i]]
    need_check$check[i] <- "not_found"
  }
}

need_check

vaild_itis_spp <- filter(need_check,check == "valid")
head(vaild_itis_spp)

write.csv(vaild_itis_spp,"result/vaild_itis_spp.csv",  row.names = F)

##################
###### gbif ######
##################

# fill in your gbif.org credentials 
user <- "ccxianren" # your gbif.org username 
pwd <- "ccxianrenc910613" # your gbif.org password
email <- "chengchen0613@gmail.com" # your email 


rgbif::occ_download

##################
###### gbif ######
##################

