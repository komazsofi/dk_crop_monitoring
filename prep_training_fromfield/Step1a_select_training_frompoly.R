library(sf)
library(dplyr)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/")
poly=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/ZsofiaData2011_LandsatTrainingData/IMK2011_XY_CropAggregationCarbon_Repair.shp")

# inverse buffer

poly_inv_buff=st_buffer(poly,-50)

# correct attributes

poly_inv_buff_c=poly_inv_buff[is.na(poly_inv_buff$AggCarbon)=='FALSE',]

poly_inv_buff_c$AggCarbon[poly_inv_buff_c$AggCarbon=='Miaze']<-"Maize"
poly_inv_buff_c$AggCarbon[poly_inv_buff_c$AggCarbon=="Patato"]<-"Potato"
poly_inv_buff_c2 = poly_inv_buff_c[!st_is_empty(poly_inv_buff_c),drop=FALSE]

#st_write(poly_inv_buff_c2,"poly_inv_buff50_c.shp")

# random stratified sampling

classes=unique(poly_inv_buff_c2$AggCarbon)

for (i in 1:length(classes)) {
  
  class=classes[i]
  
  poly_inv_buff_c2_class1=poly_inv_buff_c2[poly_inv_buff_c2$AggCarbon==class,]
  poly_inv_buff_c2_class1_sampled=st_sample(poly_inv_buff_c2_class1,500,type="random")
  
  poly_inv_buff_c2_class1_sampled_df <- as.data.frame(cbind(poly_inv_buff_c2_class1_sampled))
  names(poly_inv_buff_c2_class1_sampled_df)<-"geometry"
  poly_inv_buff_c2_class1_sampled_df$class<-class
  
  poly_inv_buff_c2_class1_sampled_df_shp=st_as_sf(poly_inv_buff_c2_class1_sampled_df)
  
  st_write(poly_inv_buff_c2_class1_sampled_df_shp,paste0(class,"_500rand.shp"))
  
}

