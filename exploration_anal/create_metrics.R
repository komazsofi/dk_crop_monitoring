library(stringr)
library(dplyr)

library(ggplot2)
library(tidyverse)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/AggrTrainingClasses/")

database=read.csv("allcsv_df_sublt05_2011.csv")

# calculate indices and it is statistics over the year

database$SR=database$SR_B4/database$SR_B3
database$NDVI=(database$SR_B4-database$SR_B3)/(database$SR_B4+database$SR_B3)
database$CIgreen=(database$SR_B4-database$SR_B2)/database$SR_B2
database$GRI=database$SR_B2/database$SR_B3
database$mNDVI=(database$SR_B4-database$SR_B3)/((database$SR_B4+database$SR_B3-2)*database$SR_B1)
database$NDWI=(database$SR_B2-database$SR_B4)/(database$SR_B2+database$SR_B4)

# calc stat

database_yearstat=database %>% 
  group_by(class,profileID) %>%
  dplyr::summarize(med_SR = median(SR),sd_SR = sd(SR),q10_SR=quantile(SR,.10),q90_SR=quantile(SR,.90),
                   med_NDVI = median(NDVI),sd_NDVI = sd(NDVI),q10_NDVI=quantile(NDVI,.10),q90_NDVI=quantile(NDVI,.90),
                   med_CIgreen = median(CIgreen),sd_CIgreen = sd(CIgreen),q10_CIgreen=quantile(CIgreen,.10),q90_CIgreen=quantile(CIgreen,.90),
                   med_GRI = median(GRI),sd_GRI = sd(GRI),q10_GRI=quantile(GRI,.10),q90_GRI=quantile(GRI,.90),
                   med_mNDVI = median(mNDVI),sd_mNDVI = sd(mNDVI),q10_mNDVI=quantile(mNDVI,.10),q90_mNDVI=quantile(mNDVI,.90),
                   med_NDWI = median(NDWI),sd_NDWI = sd(NDWI),q10_NDWI=quantile(NDWI,.10),q90_NDWI=quantile(NDWI,.90))

# month 1-6

database_part1=database[database$month<7,]

database_yearstat_part1=database_part1 %>% 
  group_by(class,profileID) %>%
  dplyr::summarize(med_SR_p1 = median(SR),sd_SR_p1 = sd(SR),q10_SR_p1=quantile(SR,.10),q90_SR_p1=quantile(SR,.90),
                   med_NDVI_p1 = median(NDVI),sd_NDVI_p1 = sd(NDVI),q10_NDVI_p1=quantile(NDVI,.10),q90_NDVI_p1=quantile(NDVI,.90),
                   med_CIgreen_p1 = median(CIgreen),sd_CIgreen_p1 = sd(CIgreen),q10_CIgreen_p1=quantile(CIgreen,.10),q90_CIgreen_p1=quantile(CIgreen,.90),
                   med_GRI_p1 = median(GRI),sd_GRI_p1 = sd(GRI),q10_GRI_p1=quantile(GRI,.10),q90_GRI_p1=quantile(GRI,.90),
                   med_mNDVI_p1 = median(mNDVI),sd_mNDVI_p1 = sd(mNDVI),q10_mNDVI_p1=quantile(mNDVI,.10),q90_mNDVI_p1=quantile(mNDVI,.90),
                   med_NDWI_p1 = median(NDWI),sd_NDWI_p1 = sd(NDWI),q10_NDWI_p1=quantile(NDWI,.10),q90_NDWI_p1=quantile(NDWI,.90))

# month 6-12

database_part2=database[database$month>6,]

database_yearstat_part2=database_part2 %>% 
  group_by(class,profileID) %>%
  dplyr::summarize(med_SR_p2 = median(SR),sd_SR_p2 = sd(SR),q10_SR_p2=quantile(SR,.10),q90_SR_p2=quantile(SR,.90),
                   med_NDVI_p2 = median(NDVI),sd_NDVI_p2 = sd(NDVI),q10_NDVI_p2=quantile(NDVI,.10),q90_NDVI_p2=quantile(NDVI,.90),
                   med_CIgreen_p2 = median(CIgreen),sd_CIgreen_p2 = sd(CIgreen),q10_CIgreen_p2=quantile(CIgreen,.10),q90_CIgreen_p2=quantile(CIgreen,.90),
                   med_GRI_p2 = median(GRI),sd_GRI_p2 = sd(GRI),q10_GRI_p2=quantile(GRI,.10),q90_GRI_p2=quantile(GRI,.90),
                   med_mNDVI_p2 = median(mNDVI),sd_mNDVI_p2 = sd(mNDVI),q10_mNDVI_p2=quantile(mNDVI,.10),q90_mNDVI_p2=quantile(mNDVI,.90),
                   med_NDWI_p2 = median(NDWI),sd_NDWI_p2 = sd(NDWI),q10_NDWI_p2=quantile(NDWI,.10),q90_NDWI_p2=quantile(NDWI,.90))

# merge metrics

db_1=merge(database_yearstat,database_yearstat_part1,by=c("class","profileID"))
db_full=merge(db_1,database_yearstat_part2,by=c("class","profileID"))

write.csv(db_full,"forRF_try1.csv")