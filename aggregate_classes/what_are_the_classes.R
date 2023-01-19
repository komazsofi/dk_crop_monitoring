library(sf)
library(tidyverse)
library(openxlsx)
library(stringi)

# import
setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/determine_classes_1/")
poly_agrec=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/ZsofiaData2011_LandsatTrainingData/Marker_2021_Slut.shp")
poly_ecol=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/BES_NATURTYPER_SHAPE/BES_NATURTYPER.shp")
poly_mette_class=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/ZsofiaData2011_LandsatTrainingData/IMK2011_XY_CropAggregationCarbon_Repair.shp")

gregor_class=read.xlsx(xlsxFile = "O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/appendix.xlsx", sheet = 1, skipEmptyRows = FALSE)

# extract individual classes and area

attArea <- poly_agrec %>% 
  mutate(area = st_area(.) %>% as.numeric())

crop_classes_all=attArea %>% 
  as_tibble() %>% 
  group_by(Afgkode,Afgroede) %>% 
  summarize(area = sum(area)/1000000)

mette_class=poly_mette_class %>% 
  as_tibble() %>% 
  group_by(CropCode,AggCarbon,EnglishTxt) %>% 
  summarize(area = sum(AREAL_HA)/1000000)

names(crop_classes_all) <- c("code","danish.name","area")
names(mette_class) <- c("code","Aggcarbon","english.name","area")

crop_class_comb=merge(mette_class, gregor_class, by = "code")

crop_class_comb_clean=crop_class_comb[(c(1,5,6,2,8,9,10))]
crop_class_comb_clean$danish.name<-iconv(crop_class_comb_clean$danish.name, to="UTF-8")
write.xlsx(crop_class_comb_clean, "O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/determine_classes_1/crop_class_comb_clean.xlsx")