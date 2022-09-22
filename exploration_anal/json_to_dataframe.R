library(RJSONIO)

library(stringr)
library(dplyr)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis/important_crops/profiles_winterwheat/profiles/LANDSAT_COMBINED_C02_T1_L2/3560277908543725591/")

jsonlist=list.files(pattern="'*.json")

class="Winterwheat"

for (i in 1:length(jsonlist)) {
  
  data <- fromJSON(jsonlist[2],nullValue = NA)
  data_c <- data.frame(matrix(unlist(data), nrow=length(data), byrow=TRUE))
  
  data_c_df=data_c[-1,]
  
  names(data_c_df)<-as.character(unlist(data_c[1,]))
  data_c_df$profileID<-i
  data_c_df$class<-class
  
  write.csv(data_c_df,paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis/important_crops/profiles_winterwheat/profiles/LANDSAT_COMBINED_C02_T1_L2/winterwheat/",i,"_",class,".csv"))
  
  
}

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis/important_crops/profiles_winterwheat/profiles/LANDSAT_COMBINED_C02_T1_L2/winterwheat/")

files <- list.files(pattern = '*.csv')

allcsv <- lapply(files,function(j){
  read.csv(j, header=TRUE)
})

allcsv_df <- do.call(rbind.data.frame, allcsv)

# select only LT05

allcsv_df_sublt05 <- allcsv_df[grep("LT05", allcsv_df$id), ]

# based on ID mine information

