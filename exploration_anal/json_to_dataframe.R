library(RJSONIO)

library(stringr)
library(dplyr)

classes=c("Beets","Cereal","Christmas trees","Environment","Forest","Fruit tree","Grass in rotation","Maize","Natural areas","Other","Permanent grass",
          "Permanent grass - set aside","Potato","Spring cereal","Suggar beats","Winter cereal","Winter rape")

for (l in 1:length(classes)) {
  
  print(classes[l])
  
  wd=paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/AggrTrainingClasses/",classes[l],"/profiles/LANDSAT_LT05_C02_T1_L2/-560600805379309072/")
  
  setwd(wd)
  
  jsonlist=list.files(pattern="'*.json")
  
  for (i in 1:length(jsonlist)) {
    
    data <- fromJSON(jsonlist[i],nullValue = NA)
    data_c <- data.frame(matrix(unlist(data), nrow=length(data), byrow=TRUE))
    
    data_c_df=data_c[-1,]
    
    names(data_c_df)<-as.character(unlist(data_c[1,]))
    data_c_df$profileID<-i
    data_c_df$class<-classes[l]
    
    write.csv(data_c_df,paste0(wd,i,"_",classes[l],".csv"))
    
    
  }
  
  files <- list.files(pattern = '*.csv')
  
  allcsv <- lapply(files,function(j){
    read.csv(j, header=TRUE)
  })
  
  allcsv_df <- do.call(rbind.data.frame, allcsv)
  
  write.csv(allcsv_df,paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/AggrTrainingClasses/",classes[l],"_all.csv"))
  
}





