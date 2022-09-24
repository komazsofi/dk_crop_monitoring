library(stringr)
library(dplyr)

library(ggplot2)
library(tidyverse)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis/important_crops/")

# import

files <- list.files(pattern = '*.csv')

allcsv <- lapply(files,function(j){
  read.csv(j, header=TRUE)
})

allcsv_df <- do.call(rbind.data.frame, allcsv)

# select only LT05

allcsv_df_sublt05 <- allcsv_df[grep("LT05", allcsv_df$id), ]

# based on ID mine information

allcsv_df_sublt05$year=substring(allcsv_df_sublt05$id,21,24)
allcsv_df_sublt05$month=substring(allcsv_df_sublt05$id,25,26)
allcsv_df_sublt05$day=substring(allcsv_df_sublt05$id,27,28)

allcsv_df_sublt05$date=substring(allcsv_df_sublt05$id,21,28)
allcsv_df_sublt05$date_c=as.Date(allcsv_df_sublt05$date,"%Y%m%d")
allcsv_df_sublt05$doy=as.numeric(format(allcsv_df_sublt05$date_c, format="%j"))

allcsv_df_sublt05_2011=allcsv_df_sublt05[allcsv_df_sublt05$year==2011,]
allcsv_df_sublt05_2011=allcsv_df_sublt05_2011[is.na(allcsv_df_sublt05_2011$NDVI)==FALSE,]

# make per class

allcsv_df_sublt05_2011_ww=allcsv_df_sublt05_2011[allcsv_df_sublt05_2011$class=="Winterwheat",]
allcsv_df_sublt05_2011_sm=allcsv_df_sublt05_2011[allcsv_df_sublt05_2011$class=="silomaize",]
allcsv_df_sublt05_2011_grass=allcsv_df_sublt05_2011[allcsv_df_sublt05_2011$class=="permanent grass with normal yield",]

allcsv_df_sublt05_2011_ndvistatww=allcsv_df_sublt05_2011_ww %>% 
  group_by(month = lubridate::floor_date(allcsv_df_sublt05_2011_ww$date_c, 'month')) %>%
  dplyr::summarize(med_NDVI = median(NDVI, na.rm = TRUE),sd_NDVI = sd(NDVI, na.rm = TRUE))

allcsv_df_sublt05_2011_ndvistatsm=allcsv_df_sublt05_2011_sm %>% 
  group_by(month = lubridate::floor_date(allcsv_df_sublt05_2011_sm$date_c, 'month')) %>%
  dplyr::summarize(med_NDVI = median(NDVI, na.rm = TRUE),sd_NDVI = sd(NDVI, na.rm = TRUE))

allcsv_df_sublt05_2011_ndvistatgrass=allcsv_df_sublt05_2011_grass %>% 
  group_by(month = lubridate::floor_date(allcsv_df_sublt05_2011_grass$date_c, 'month')) %>%
  dplyr::summarize(med_NDVI = median(NDVI, na.rm = TRUE),sd_NDVI = sd(NDVI, na.rm = TRUE))

ggplot(allcsv_df_sublt05_2011_ndvistatww,aes(x=month,y=med_NDVI))+
  geom_point(color="goldenrod4",size=3)+geom_line(color="goldenrod4",size=3)+theme_bw(base_size = 25)+
  geom_ribbon(aes(ymin=med_NDVI-sd_NDVI, ymax=med_NDVI+sd_NDVI), fill = "grey70",alpha = 0.25)+
  #geom_errorbar(aes(ymin=mean_NDVI-sd_NDVI, ymax=mean_NDVI+sd_NDVI),color="goldenrod4",size=1)+
  xlab("Months in the year")

ggplot(allcsv_df_sublt05_2011_ndvistatsm,aes(x=month,y=med_NDVI))+
  geom_point(color="goldenrod2",size=3)+geom_line(color="goldenrod2",size=3)+theme_bw(base_size = 25)+
  geom_ribbon(aes(ymin=med_NDVI-sd_NDVI, ymax=med_NDVI+sd_NDVI), fill = "grey70",alpha = 0.25)+
  #geom_errorbar(aes(ymin=mean_NDVI-sd_NDVI, ymax=mean_NDVI+sd_NDVI),color="goldenrod2",size=1)+
  xlab("Months in the year")

ggplot(allcsv_df_sublt05_2011_ndvistatgrass,aes(x=month,y=med_NDVI))+
  geom_point(color="darkolivegreen4",size=3)+geom_line(color="darkolivegreen4",size=3)+theme_bw(base_size = 25)+
  geom_ribbon(aes(ymin=med_NDVI-sd_NDVI, ymax=med_NDVI+sd_NDVI), fill = "grey70",alpha = 0.25)+
  #geom_errorbar(aes(ymin=mean_NDVI-sd_NDVI, ymax=mean_NDVI+sd_NDVI),color="darkolivegreen4",size=1)+
  xlab("Months in the year")


# make stat per month across all

allcsv_df_sublt05_2011_ndvistat=allcsv_df_sublt05_2011 %>% 
  group_by(month = lubridate::floor_date(allcsv_df_sublt05_2011$date_c, 'month'),class) %>%
  dplyr::summarize(med_NDVI = median(NDVI, na.rm = TRUE),sd_NDVI = sd(NDVI, na.rm = TRUE))

# visualize

ggplot(allcsv_df_sublt05_2011_ndvistat,aes(x=month,y=med_NDVI,color=class))+
  geom_point(size=3)+geom_line(size=3)+theme_bw(base_size = 25)+
  xlab("Months in the year")+
  scale_color_manual(values=c("darkolivegreen4", "goldenrod2", "goldenrod4"),labels = c("Perm, Grass", "Silo Maize", "Winter weat"))

# one particular selected center

# quality, amount landsat available

allcsv_df_sublt05_2011_nstat=allcsv_df_sublt05_2011 %>% 
  group_by(profileID,class,longitude,latitude) %>%
  dplyr::summarize(n=n(),min_date=min(date),max_date=max(date),range_date=max(doy)-min(doy))

write.csv(allcsv_df_sublt05_2011_nstat,"allcsv_df_sublt05_2011_stat.csv")








