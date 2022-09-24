library(RJSONIO)

library(stringr)
library(dplyr)

class="permanent grass with normal yield"
wd="O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis/important_crops/profile_permgrass/profiles/LANDSAT_COMBINED_C02_T1_L2/2739802740976160412/"

setwd(wd)

jsonlist=list.files(pattern="'*.json")

for (i in 1:length(jsonlist)) {
  
  data <- fromJSON(jsonlist[i],nullValue = NA)
  data_c <- data.frame(matrix(unlist(data), nrow=length(data), byrow=TRUE))
  
  data_c_df=data_c[-1,]
  
  names(data_c_df)<-as.character(unlist(data_c[1,]))
  data_c_df$profileID<-i
  data_c_df$class<-class
  
  write.csv(data_c_df,paste0(wd,i,"_",class,".csv"))
  
  
}

setwd(wd)

files <- list.files(pattern = '*.csv')

allcsv <- lapply(files,function(j){
  read.csv(j, header=TRUE)
})

allcsv_df <- do.call(rbind.data.frame, allcsv)

write.csv(allcsv_df,paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis/important_crops/",class,"_all.csv"))





