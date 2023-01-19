library(sf)
library(tidyverse)
library(openxlsx)
library(stringi)

# import
setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/determine_classes_1/")
poly_agrec=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/ZsofiaData2011_LandsatTrainingData/Marker_2021_Slut.shp")
poly_ecol=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/BES_NATURTYPER_SHAPE/BES_NATURTYPER.shp")

# select ph3 categories one-by-one

ph3=c("Strandeng","Eng","Mose","Overdrev","Hede")

for(i in 1:5) {
  
  print(ph3[i])
  
  class = poly_ecol[poly_ecol$Natyp_navn == ph3[i],]
  
  poly_int <- st_intersection(poly_agrec, class)
  poly_int_warea <- poly_int %>% mutate(area = st_area(.) %>% as.numeric())
  
  poly_int_warea_clean <- st_collection_extract(poly_int_warea, "POLYGON")
  st_write(poly_int_warea_clean,paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/determine_classes_1/poly_int_warea_clean",ph3[i],".shp"))
  
  # aggregate
  
  intersect_stat=poly_int_warea %>% 
    as_tibble() %>% 
    group_by(Afgroede, Natyp_navn) %>% 
    summarize(area = sum(area)/10000)
  
  intersect_stat$Natyp_navn<-iconv(intersect_stat$Natyp_navn, to="UTF-8")
  write.xlsx(intersect_stat, paste0("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/determine_classes_1/poly_int_warea_clean",ph3[i],".xlsx"))
  
}

