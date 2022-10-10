library(sf)
library(tidyverse)
library(openxlsx)
library(stringi)

# import
setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/")
poly_agrec=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/ZsofiaData2011_LandsatTrainingData/Marker_2021_Slut.shp")
poly_ecol=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/BES_NATURTYPER_SHAPE/BES_NATURTYPER.shp")

# intersection

poly_int <- st_intersection(poly_agrec, poly_ecol)
poly_single=st_collection_extract(poly_int, "POLYGON")

# calc stat

attArea <- poly_int %>% 
  mutate(area = st_area(.) %>% as.numeric())

cross_classnames=attArea %>% 
  as_tibble() %>% 
  group_by(Afgroede, Natyp_navn) %>% 
  summarize(area = sum(area)/1000000)

# export

poly_single_sel=poly_single[,c(15:44,6,9,11,45)]
st_write(poly_single_sel,"polygon_ecol_intersected_agroecology.shp")

cross_classnames$Natyp_navn<-iconv(cross_classnames$Natyp_navn, to="UTF-8")
write.xlsx(cross_classnames, "cross_classnames.xlsx")

