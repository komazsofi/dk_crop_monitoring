library(sf)
library(dplyr)
library(ggplot2)
library(units)
library(randomcoloR)

setwd("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/exploration_analysis_2/")
poly=st_read("O:/Nat_Sustain-proj/_user/ZsofiaKoma_au700510/Landsat_crop/ZsofiaData2011_LandsatTrainingData/IMK2011_XY_CropAggregationCarbon_Repair.shp")

poly_c=poly[is.na(poly$AggCarbon)=='FALSE',]

poly_c$AggCarbon[poly_c$AggCarbon=='Miaze']<-"Maize"
poly_c$AggCarbon[poly_c$AggCarbon=="Patato"]<-"Potato"

# calculate area aggregated classes

poly_c$area <- st_area(poly_c)

polyareas=poly_c %>% 
  st_drop_geometry() %>%
  group_by(AggCarbon) %>% 
  summarise(area = sum(area))

polyareas$perc=(polyareas$area/sum(polyareas$area))*100

ggplot(polyareas, aes(x=reorder(AggCarbon,perc), y=perc, fill=AggCarbon)) + 
  geom_bar(stat="identity",show.legend = FALSE) +
  coord_flip()+
  theme_bw(base_size = 25)+
  ylab("Percentage of area to all")+xlab("Aggregated classes")+
  scale_fill_manual(values=distinctColorPalette(length(unique(reorder(polyareas$AggCarbon,polyareas$perc)))))

# calculate for all existing classes

polyareas_finest=poly_c %>% 
  st_drop_geometry() %>%
  group_by(EnglishTxt) %>% 
  summarise(area = sum(area))

polyareas_finest$perc=(polyareas_finest$area/sum(polyareas_finest$area))*100
polyareas_finest_order <-polyareas_finest[order(polyareas_finest$perc,decreasing = TRUE),]

polyareas_finest_order$sums <- cumsum(polyareas_finest_order$perc)

ggplot(polyareas_finest_order[1:25,], aes(x=reorder(EnglishTxt,perc), y=perc, fill=EnglishTxt)) + 
  geom_bar(stat="identity",show.legend = FALSE) +
  coord_flip()+
  theme_bw(base_size = 25)+
  ylab("Percentage of area to all")+xlab("Aggregated classes")+
  scale_fill_manual(values=distinctColorPalette(length(unique(polyareas_finest_order$EnglishTxt))))