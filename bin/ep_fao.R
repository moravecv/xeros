library(readxl)
library(data.table)
library(reshape2)
library(pracma)
library(zoo)
library(ggplot2)
library(rgdal)
library(BAMMtools)
library(RColorBrewer)
library(ggplot2)
library(data.table)
library(dplyr)
library(cowplot)
library(ggrepel)

pp <- function(p, N){((p - 0.3)/(N + 0.4))}

folder <- "D:/XEROS/git/xeros/data/crops_2018_2020/"

########## WHEAT 2020 ###############

wheat_prod <- data.table(read_excel(paste0(folder,"TAG00047_n.xlsx"), 
                                    col_types = c("text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), na = ":"))

wheat_area <- data.table(read_excel(paste0(folder,"TAG00047_a.xlsx"), 
                                    col_types = c("text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), na = ":"))

wheat_yield <- cbind(TIME = as.data.frame(wheat_prod)[,1], round(as.data.frame(wheat_prod)[,c(2:ncol(wheat_prod))] / as.data.frame(wheat_area)[,c(2:ncol(wheat_area))] *10000, digits = 0))

wheat_prod <- wheat_prod[, .(TIME, `2020`)]
wheat_area <- wheat_area[, .(TIME, `2020`)]

wheat <- merge(x = wheat_area, y =  wheat_prod, by = "TIME")
wheat$yield <- round(wheat$`2020.y`/ wheat$`2020.x` * 10000, digits = 0)
wheat$`2020.x` <- wheat$`2020.y` <- NULL
colnames(wheat) <- c("TIME",  "2020")
wheat$crop <- "Wheat"

########## GRAIN 2020 ############

grain_prod <- data.table(read_excel(paste0(folder,"TAG00093_n.xlsx"), 
                                    col_types = c("text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), na = ":"))

grain_area <- data.table(read_excel(paste0(folder,"TAG00093_a.xlsx"), 
                                    col_types = c("text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric"), na = ":"))

grain_yield <- data.table(cbind(TIME = as.data.frame(grain_prod)[,1], round(as.data.frame(grain_prod)[,c(2:ncol(grain_prod))] / as.data.frame(grain_area)[,c(2:ncol(grain_area))] *10000, digits = 0)))

grain_prod <- grain_prod[, .(TIME, `2020`)]
grain_area <- grain_area[, .(TIME, `2020`)]

grain <- merge(x = grain_area, y =  grain_prod, by = "TIME")
grain$yield <- round(grain$`2020.y`/ grain$`2020.x` * 10000, digits = 0)
grain$`2020.x` <- grain$`2020.y` <- NULL
colnames(grain) <- c("TIME",  "2020")
grain$crop <- "Grain"

########## BARLEY 2020 ############

barley_prod <- data.table(read_excel(paste0(folder,"TAG00051_n.xlsx"), 
                                     col_types = c("text", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric"), na = ":"))

barley_area <- data.table(read_excel(paste0(folder,"TAG00051_a.xlsx"), 
                                     col_types = c("text", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric"), na = ":"))

barley_yield <- data.table(cbind(TIME = as.data.frame(barley_prod)[,1], round(as.data.frame(barley_prod)[,c(2:ncol(barley_prod))] / as.data.frame(barley_area)[,c(2:ncol(barley_area))] *10000, digits = 0)))

barley_prod <- barley_prod[, .(TIME, `2020`)]
barley_area <- barley_area[, .(TIME, `2020`)]

barley <- merge(x = barley_area, y =  barley_prod, by = "TIME")
barley$yield <- round(barley$`2020.y`/ barley$`2020.x` * 10000, digits = 0)
barley$`2020.x` <- barley$`2020.y` <- NULL
colnames(barley) <- c("TIME",  "2020")
barley$crop <- "Barley"

########## ALL 2020 ##############

all_crops <- rbind(wheat, grain, barley)

################# FAO ####################

fao <- data.table(read.table(file = paste0(folder,"FAOSTAT_data_6-4-2021_yield.csv"), header = T, sep = ","))

fao$Area <- as.character(fao$Area)

fao[Area == "Belgium-Luxembourg", Area:= "Belgium"]
fao[Area == "United Kingdom of Great Britain and Northern Ireland", Area:= "United Kingdom"]
fao <- fao[Area %in% wheat$TIME,]

fao$ï..Domain.Code <- fao$Domain <-  fao$Area.Code..FAO. <- fao$Element.Code <- fao$Element <- fao$Year.Code <- fao$Unit <- fao$Flag <- fao$Flag.Description <- fao$Item.Code..FAO. <- NULL

###################### WHEAT ###################

fao_wheat <- fao[Item == "Wheat"]

fao_wheat_dcast <- dcast(fao_wheat, Area+Item~Year, value.var = "Value")

fao_wheat_dcast <- merge(x = fao_wheat_dcast, y = all_crops[crop == "Wheat", .(TIME, `2020`)], by.x = "Area", by.y = "TIME")

fao_wheat_m <- data.table(melt(fao_wheat_dcast,id.vars = c("Area", "Item")))

colnames(fao_wheat_m) <- c("area","item","year", "value")
fao_wheat_m$year <- as.numeric(as.character(fao_wheat_m$year))

setkey(x = fao_wheat_m)

######### ep #######

fao_wheat_m[complete.cases(fao_wheat_m[,list(value,year)]) , detrend:= residuals(lm(value ~ year, .SD)), by = area ]
fao_wheat_m[ , p:= (frank(detrend, na.last = 'keep') - .3)/(.N + 0.4), by = area]
fao_wheat_m[ , rollmean:=rollmean(p, 3, na.rm =T, align = "center", fill = NA), by = area]

countries <- unique(fao_wheat_m$area)
probabs <- c(2,5,10,20,30,40,50)
list_probabs <- list()
for (zz in c(1:length(countries))){
  nn = nrow(fao_wheat_m[!is.na(value) & area == countries[zz]])
  vec <- c(0)
  for (k in 1:7){
    k2 <- k + 1
    vec[k2] <- pp(p = probabs[k], N = nn)
  }
  vec[k2+1] <- Inf
  list_probabs[[zz]] <- vec
}


for (i in c(1:length(countries))){
  fao_wheat_m[area == countries[i], lrnk := cut(rollmean, 
                                                breaks = list_probabs[[i]], 
                                                lab = c('<2', '5', '10', '20', '30', '40', '50', '>50')
  )]
}

wheat_2020 <- fao_wheat_m[year == 2019]

###################### GRAIN ###################

fao_grain <- fao[Item == "Maize"]

fao_grain_dcast <- data.table(dcast(fao_grain, Area+Item~Year, value.var = "Value"))

fao_grain_dcast[Area == "Sweden", `2010`:= grain_yield[TIME == "Sweden", `2010`]]
fao_grain_dcast[Area == "Sweden", `2011`:= grain_yield[TIME == "Sweden", `2011`]]
fao_grain_dcast[Area == "Sweden", `2012`:= grain_yield[TIME == "Sweden", `2012`]]
fao_grain_dcast[Area == "Sweden", `2013`:= grain_yield[TIME == "Sweden", `2013`]]
fao_grain_dcast[Area == "Sweden", `2014`:= grain_yield[TIME == "Sweden", `2014`]]
fao_grain_dcast[Area == "Sweden", `2015`:= grain_yield[TIME == "Sweden", `2015`]]
fao_grain_dcast[Area == "Sweden", `2016`:= grain_yield[TIME == "Sweden", `2016`]]
fao_grain_dcast[Area == "Sweden", `2017`:= grain_yield[TIME == "Sweden", `2017`]]
fao_grain_dcast[Area == "Sweden", `2018`:= grain_yield[TIME == "Sweden", `2018`]]
fao_grain_dcast[Area == "Sweden", `2019`:= grain_yield[TIME == "Sweden", `2019`]]


fao_grain_dcast[Area == "United Kingdom", `2012`:= grain_yield[TIME == "United Kingdom", `2012`]]
fao_grain_dcast[Area == "United Kingdom", `2013`:= grain_yield[TIME == "United Kingdom", `2013`]]
fao_grain_dcast[Area == "United Kingdom", `2015`:= grain_yield[TIME == "United Kingdom", `2015`]]
fao_grain_dcast[Area == "United Kingdom", `2016`:= grain_yield[TIME == "United Kingdom", `2016`]]
fao_grain_dcast[Area == "United Kingdom", `2017`:= grain_yield[TIME == "United Kingdom", `2017`]]
fao_grain_dcast[Area == "United Kingdom", `2018`:= grain_yield[TIME == "United Kingdom", `2018`]]
fao_grain_dcast[Area == "United Kingdom", `2019`:= grain_yield[TIME == "United Kingdom", `2019`]]

fao_grain_dcast <- merge(x = fao_grain_dcast, y = all_crops[crop == "Grain", .(TIME, `2020`)], by.x = "Area", by.y = "TIME")

fao_grain_m <- data.table(melt(fao_grain_dcast,id.vars = c("Area", "Item")))

colnames(fao_grain_m) <- c("area","item","year", "value")
fao_grain_m$year <- as.numeric(as.character(fao_grain_m$year))

setkey(x = fao_grain_m)

######### ep #######

fao_grain_m[complete.cases(fao_grain_m[,list(value,year)]) , detrend:= residuals(lm(value ~ year, .SD)), by = area ]
fao_grain_m[ , p:= (frank(detrend, na.last = 'keep') - .3)/(.N + 0.4), by = area]
fao_grain_m[ , rollmean:=rollmean(p, 3, na.rm =T, align = "center", fill = NA), by = area]

countries <- unique(fao_grain_m$area)
probabs <- c(2,5,10,20,30,40,50)
list_probabs <- list()
for (zz in c(1:length(countries))){
  nn = nrow(fao_grain_m[!is.na(value) & area == countries[zz]])
  vec <- c(0)
  for (k in 1:7){
    k2 <- k + 1
    vec[k2] <- pp(p = probabs[k], N = nn)
  }
  vec[k2+1] <- Inf
  list_probabs[[zz]] <- vec
}


for (i in c(1:length(countries))){
  fao_grain_m[area == countries[i], lrnk := cut(rollmean, 
                                                breaks = list_probabs[[i]], 
                                                lab = c('<2', '5', '10', '20', '30', '40', '50', '>50')
  )]
}

grain_2020 <- fao_grain_m[year == 2019]

###################### BARLEY ###################

fao_barley <- fao[Item == "Barley"]

fao_barley_dcast <- dcast(fao_barley, Area+Item~Year, value.var = "Value")

fao_barley_dcast <- merge(x = fao_barley_dcast, y = all_crops[crop == "Barley", .(TIME, `2020`)], by.x = "Area", by.y = "TIME")

fao_barley_m <- data.table(melt(fao_barley_dcast,id.vars = c("Area", "Item")))

colnames(fao_barley_m) <- c("area","item","year", "value")
fao_barley_m$year <- as.numeric(as.character(fao_barley_m$year))

setkey(x = fao_barley_m)

######### ep #######

fao_barley_m[complete.cases(fao_barley_m[,list(value,year)]) , detrend:= residuals(lm(value ~ year, .SD)), by = area ]
fao_barley_m[ , p:= (frank(detrend, na.last = 'keep') - .3)/(.N + 0.4), by = area]
fao_barley_m[ , rollmean:=rollmean(p, 3, na.rm =T, align = "center", fill = NA), by = area]

countries <- unique(fao_barley_m$area)
probabs <- c(2,5,10,20,30,40,50)
list_probabs <- list()
for (zz in c(1:length(countries))){
  nn = nrow(fao_barley_m[!is.na(value) & area == countries[zz]])
  vec <- c(0)
  for (k in 1:7){
    k2 <- k + 1
    vec[k2] <- pp(p = probabs[k], N = nn)
  }
  vec[k2+1] <- Inf
  list_probabs[[zz]] <- vec
}


for (i in c(1:length(countries))){
  fao_barley_m[area == countries[i], lrnk := cut(rollmean, 
                                                 breaks = list_probabs[[i]], 
                                                 lab = c('<2', '5', '10', '20', '30', '40', '50', '>50')
  )]
}

barley_2020 <- fao_barley_m[year == 2019]

################ ALL DATA ##################

all_data <- rbind(wheat_2020, grain_2020, barley_2020)
saveRDS(object = all_data, file = paste0(folder,"EP_data.rds"))

################## PLOT ################
eu <- readRDS('D:/XEROS/git/xeros/data/geo/Europe.rds')

xlm_new <- c(2650000,5870000)
ylm_new <- c(1550000,5260000)

shp.names <- readRDS("D:/PAPERS/Seasaw_2018/GEO/names.rds")
shp <- readOGR(dsn = "D:/PAPERS/Seasaw_2018/GEO/euCopy.shp", layer = "euCopy")
shp <- spTransform(shp, proj4string(obj = eu))

all_data[area == "Czechia", area:=	"Czech Republic"]
colnames(all_data) <- c("id", "item", "year", "value", "detrend", "p", "rollmean", "lrnk")

shp_df <- broom::tidy(shp, region = "CNTRY_NAME")
cnames <- aggregate(cbind(long, lat) ~ id, data = shp_df, FUN=mean)

shp_wheat <- left_join(x = as.data.frame(shp_df), y = as.data.frame(all_data[item == "Wheat"]), by = c("id"))
shp_grain <- left_join(x = as.data.frame(shp_df), y = as.data.frame(all_data[item == "Maize"]), by = c("id"))
shp_barley <- left_join(x = as.data.frame(shp_df), y = as.data.frame(all_data[item == "Barley"]), by = c("id"))

########### LABELS ##############

centroid_wheat <- aggregate(cbind(long,lat) ~ id, data=shp_wheat, FUN=mean)
centroid_grain <- aggregate(cbind(long,lat) ~ id, data=shp_grain, FUN=mean)
centroid_barley <- aggregate(cbind(long,lat) ~ id, data=shp_barley, FUN=mean)

fao_wheat_m[area == "Czechia", area:= "Czech Republic"]
fao_grain_m[area == "Czechia", area:= "Czech Republic"]
fao_barley_m[area == "Czechia", area:= "Czech Republic"]

for (zz in c(1:nrow(centroid_wheat))){
  cc <- centroid_wheat[zz,1]
  nn = nrow(fao_wheat_m[!is.na(value) & area == cc])
  centroid_wheat[zz, 4] <- as.character(nn)
}

for (zz in c(1:nrow(centroid_grain))){
  cc <- centroid_grain[zz,1]
  nn = nrow(fao_grain_m[!is.na(value) & area == cc])
  centroid_grain[zz, 4] <- nn
}

for (zz in c(1:nrow(centroid_barley))){
  cc <- centroid_barley[zz,1]
  nn = nrow(fao_barley_m[!is.na(value) & area == cc])
  centroid_barley[zz, 4] <- nn
}

######## PLOT WHEAT EP ###############
(gg_wheat <- ggplot() + 
   geom_polygon(data = shp_wheat, aes(x = long, y = lat, group = group, fill = lrnk), colour = "black", size = 0.2)+
   geom_label_repel(data = centroid_wheat,
                   aes(long, lat,label = V4,group = id),
                   color = 'black', fill = "white",
                   size  = 2, min.segment.length = 0.1,
                   box.padding = 0.7, point.padding = 0.5,
                   max.time = 5, max.iter = 100000, max.overlaps = 20)+
   coord_fixed(xlim = xlm_new, ylim = ylm_new)+
   scale_fill_manual(values = c('#b10026', '#e31a1c','#fc4e2a', '#fd8d3c', '#feb24c', 
                                '#fed976', '#ffeda0'),  na.value = "grey70",
                     name = 'Exceedance probability', 
                     labels = c("<2", "2-5", "5-10", "10-20", "20-30", "30-40", "40-50"), 
                     guide = guide_legend(nrow = 1, title.position = 'top',
                                          label.position = "bottom")) + 
   theme_void()+
   theme(legend.position = "none"))

######## PLOT GRAIN EP ###############
(gg_grain <- ggplot() + 
   geom_polygon(data = shp_grain, aes(x = long, y = lat, group = group, fill = lrnk), colour = "black", size = 0.2)+
   geom_label_repel(data = centroid_grain,
                    aes(long, lat,label = V4,group = id),
                    color = 'black', fill = "white",
                    size  = 2, min.segment.length = 0.1,
                    box.padding = 0.7, point.padding = 0.5,
                    max.time = 3, max.iter = 100000, max.overlaps = 15)+
   coord_fixed(xlim = xlm_new, ylim = ylm_new)+
   scale_fill_manual(values = c('#b10026', '#e31a1c','#fc4e2a', '#fd8d3c', '#feb24c', 
                                '#ffeda0'), na.value = "grey70",
                     name = 'Exceedance probability', 
                     labels = c("<2", "2-5", "5-10", "10-20", "20-30", "40-50"), 
                     guide = guide_legend(nrow = 1, title.position = 'top',
                                          label.position = "bottom")) + 
   theme_void()+
   theme(legend.position = "none"))

######## PLOT BARLEY EP ###############
(gg_barley <- ggplot() + 
   geom_polygon(data = shp_barley, aes(x = long, y = lat, group = group, fill = lrnk), colour = "black", size = 0.2)+
   geom_label_repel(data = centroid_barley,
                    aes(long, lat,label = V4,group = id),
                    color = 'black', fill = "white",
                    size  = 2, min.segment.length = 0.1,
                    box.padding = 0.7, point.padding = 0.5,
                    max.time = 3, max.iter = 100000, max.overlaps = 15)+
   coord_fixed(xlim = xlm_new, ylim = ylm_new)+
   scale_fill_manual(values = c('#b10026', '#fc4e2a', '#fd8d3c', '#feb24c', 
                                '#fed976', '#ffeda0','#ffffcc'), na.value = "grey70",
                     name = 'Exceedance probability', 
                     #labels = c("<2", "5-10", "10-20", "20-30", "30-40", "40-50", ">50"), 
                     guide = guide_legend(nrow = 1, title.position = 'top',
                                          label.position = "bottom")) + 
   theme_void()+
   theme(legend.position = "none"))

############## PLOT LEGEND #####################

shp_plot <- data.table(shp_wheat)

shp_plot[id == "Bulgaria", lrnk:= ">50"]

(gg_plot <- ggplot() + 
   geom_polygon(data = shp_plot, aes(x = long, y = lat, group = group, fill = lrnk), colour = "black", size = 0.2)+
   #geom_label_repel(data = centroid_wheat,
    #                aes(long, lat,label = V4,group = id),
     #               color = 'black', fill = "white",
      #              size  = 3,
       #             box.padding = 0.7, point.padding = 0.5, max.time = 3)+
   coord_fixed(xlim = xlm_new, ylim = ylm_new)+
   scale_fill_manual(values = c('#b10026', '#e31a1c','#fc4e2a', '#fd8d3c', '#feb24c', 
                                '#fed976', '#ffeda0', '#ffffcc'),  na.value = "grey70",
                     name = 'Exceedance probability', 
                     labels = c("<2", "2-5", "5-10", "10-20", "20-30", "30-40", "40-50", ">50"), 
                     guide = guide_legend(nrow = 1, title.position = 'top',
                                          label.position = "bottom")) + 
   theme_void()+
   theme(legend.position = "bottom",
         legend.title = element_blank(),
         legend.direction = "horizontal",
         legend.key = element_rect(colour = "black", size = 0.001),
         legend.key.width = unit(15,"mm"), legend.key.height = unit(3, "mm"), 
         legend.text = element_text(size = 9)))

ep_lgnd <- get_legend(gg_plot)

########## MULTIPLOT ####### 
library(ggpubr)

ggarrange(                
  
  ggarrange(gg_wheat, gg_grain, gg_barley, ncol = 3), ep_lgnd,
  nrow = 2, heights = c(1, 0.1)      
) 

ggsave(filename = "C:/Users/vmoravec/Desktop/ep_new_3.pdf", device = "pdf", width = 210, height = 100, units = "mm")
