library(stringr)
library(dplyr)

library(ggplot2)
library(tidyverse)

library(randomcoloR)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/AggrTrainingClasses/")

# import

files <- list.files(pattern = '*.csv')

allcsv <- lapply(files,function(j){
  read.csv(j, header=TRUE)
})

allcsv_df <- do.call(rbind.data.frame, allcsv)

# select only LT05

allcsv_df_sublt05 <- allcsv_df[grep("LT05", allcsv_df$id), ]

# based on ID mine information

allcsv_df_sublt05$year=substr(allcsv_df_sublt05$id,13,16)
allcsv_df_sublt05$month=substring(allcsv_df_sublt05$id,17,18)
allcsv_df_sublt05$day=substring(allcsv_df_sublt05$id,19,20)

allcsv_df_sublt05$date=substring(allcsv_df_sublt05$id,13,20)
allcsv_df_sublt05$date_c=as.Date(allcsv_df_sublt05$date,"%Y%m%d")
allcsv_df_sublt05$doy=as.numeric(format(allcsv_df_sublt05$date_c, format="%j"))

allcsv_df_sublt05_2011=allcsv_df_sublt05[allcsv_df_sublt05$year==2011,]
allcsv_df_sublt05_2011=allcsv_df_sublt05_2011[is.na(allcsv_df_sublt05_2011$SR_B1)==FALSE,]

write.csv(allcsv_df_sublt05_2011,"allcsv_df_sublt05_2011.csv")

# calculate indices

allcsv_df_sublt05_2011$NDVI=(allcsv_df_sublt05_2011$SR_B4-allcsv_df_sublt05_2011$SR_B3)/(allcsv_df_sublt05_2011$SR_B4+allcsv_df_sublt05_2011$SR_B3)

# make per class

classes=unique(allcsv_df_sublt05_2011$class)
colors=distinctColorPalette(length(classes))

for (i in 1:length(classes)) {
  
  allcsv_df_sublt05_2011_sel=allcsv_df_sublt05_2011[allcsv_df_sublt05_2011$class==classes[i],]
  
  allcsv_df_sublt05_2011_ndvistat_sel=allcsv_df_sublt05_2011_sel %>% 
    group_by(month = lubridate::floor_date(allcsv_df_sublt05_2011_sel$date_c, 'month')) %>%
    dplyr::summarize(med_NDVI = median(NDVI, na.rm = TRUE),sd_NDVI = sd(NDVI, na.rm = TRUE))
  
  ggplot(allcsv_df_sublt05_2011_ndvistat_sel,aes(x=month,y=med_NDVI))+
    geom_point(color=colors[i],size=3)+geom_line(color=colors[i],size=3)+theme_bw(base_size = 25)+
    geom_ribbon(aes(ymin=med_NDVI-sd_NDVI, ymax=med_NDVI+sd_NDVI), fill = "grey70",alpha = 0.25)+
    #geom_errorbar(aes(ymin=mean_NDVI-sd_NDVI, ymax=mean_NDVI+sd_NDVI),color="goldenrod4",size=1)+
    xlab("Months in the year")+ylim(c(0,1))
  
  ggsave(paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/",classes[i],".png"))
  
}

# make stat per month across all

allcsv_df_sublt05_2011_ndvistat=allcsv_df_sublt05_2011 %>% 
  group_by(month = lubridate::floor_date(allcsv_df_sublt05_2011$date_c, 'month'),class) %>%
  dplyr::summarize(med_NDVI = median(NDVI, na.rm = TRUE),sd_NDVI = sd(NDVI, na.rm = TRUE))

# visualize

ggplot(allcsv_df_sublt05_2011_ndvistat,aes(x=month,y=med_NDVI,color=class))+
  geom_point(size=3)+geom_line(size=3)+theme_bw(base_size = 25)+
  xlab("Months in the year")

# one particular selected center

# quality, amount landsat available

allcsv_df_sublt05_2011_nstat=allcsv_df_sublt05_2011 %>% 
  group_by(profileID,class,longitude,latitude) %>%
  dplyr::summarize(n=n(),min_date=min(date),max_date=max(date),range_date=max(doy)-min(doy))

write.csv(allcsv_df_sublt05_2011_nstat,"allcsv_df_sublt05_2011_stat.csv")








