source("./source/libs.R")

eu = readRDS('./data/geo/Europe.rds')
geu = geom_path(aes(x = long, y = lat, group = group), data = eu, size = 0.2)
xlm = bbox(eu)['x', ]
ylm = bbox(eu)['y',]

dta <- readRDS('./data/dta/rs_met_001.rds')
#dta <- readRDS('./data/dta/rs_met_002.rds')
dta[is.na(REG), REG:= "NEU"]

dta[, SXI1 := qnorm((frank(value, na.last = 'keep') - .3)/(.N + 0.4)), by = .(xc, yc, LON, LAT, REG, var, month(DTM))]
dta[, SXI3 := qnorm((frank(rollmean(value, k = 3, fill = NA, align = 'right'), na.last = 'keep') - .3)/(.N + 0.4)), by = .(xc, yc, LON, LAT, REG, var, month(DTM))]
#dta[, SXI6 := qnorm((frank(rollmean(value, k = 6, fill = NA, align = 'right'), na.last = 'keep') - .3)/(.N + 0.4)), by = .(xc, yc, LON, LAT, REG, var)]

dta[, SXI := ifelse(var != 'p', SXI1, SXI3)]

dta <- dta[!var %in% c('t', 'pet'), .(xc, yc, LON, LAT, REG, DTM, var, SXI)]
gc()

dta[, incl := SXI < qnorm(0.2)]

ysta <- dta[, .(aval = sum(SXI[incl == TRUE], na.rm = TRUE)), by = .(xc, yc, LON, LAT, REG, year(DTM), var)]

rm(dta)
gc()

ysta[, agg1 := 1:.N, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg2 := (year - 1765) %/% 2, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg3 := (year - 1764) %/% 3, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg4 := (year - 1763) %/% 4, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg5 := (year - 1759) %/% 5, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg6 := (year - 1761) %/% 6, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg7 := (year - 1760) %/% 7, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg8 := (year - 1755) %/% 8, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg9 := (year - 1758) %/% 9, by =.(xc, yc, LON, LAT, REG, var)]
ysta[, agg10 := (year - 1759) %/% 10, by =.(xc, yc, LON, LAT, REG, var)]

mysta <- melt(ysta, id.vars = c("xc", "yc", "LON", "LAT", "REG", "year", "var", "aval"))
mysta[, yag := as.double(substr(variable, 4,5))]

ag <- mysta[, .(year = year[.N], agval = mean(aval, na.rm = TRUE)), by = .(xc, yc, LON, LAT, REG, var, variable,  yag, value) ]
ag[yag != 1, lab := paste0(year - yag + 1, '-', year)]
ag[yag == 1, lab := as.character(year)]
ag[, rnk := frank(agval, na.last = 'keep'), by = .(xc, yc, LON, LAT, REG, var, variable, yag)]
ag[, p := (rnk - .3)/(.N + .4), by = .(xc, yc, LON, LAT, REG, var, variable, yag)]

breaks_list <- list()

for (i in 1:10){
  breaks_list[[i]] <- c(0, pp(2,253/i), pp(5,253/i), pp(10,253/i), pp(20,253/i), 
                        pp(30,253/i), pp(40,253/i), pp(50,253/i), Inf)
}

for (i in 1:10){
  ag[yag == i, lrnk := cut(p, 
                           breaks = breaks_list[[i]], 
                           lab = c('<2', '5', '10', '20', '30', '40', '50', '>50')
  )]
}

######################################
e <- list()
for (i in c(1:10)){
  a <- data.table(ag[variable == unique(ag$variable)[i], year[which(p < breaks_list[[i]][3])], by = .(var, LON, LAT)][var=='p', table(V1)])
  a1 <- a[V1 != 2018,][N %in% sort(N, decreasing = T)[1:5],]
  a1 <- rbind(a1, a[V1 == 2018,])
  a1$VAR <- "p"
  
  b <- data.table(ag[variable == unique(ag$variable)[i], year[which(p < breaks_list[[i]][3])], by = .(var, LON, LAT)][var=='q', table(V1)])
  b1 <- b[V1 != 2018,][N %in% sort(N, decreasing = T)[1:5],]
  b1 <- rbind(b1, b[V1 == 2018,])
  b1$VAR <- "q"
  
  c <- data.table(ag[variable == unique(ag$variable)[i], year[which(p < breaks_list[[i]][3])], by = .(var, LON, LAT)][var=='s', table(V1)])
  c1 <- c[V1 != 2018,][N %in% sort(N, decreasing = T)[1:5],]
  c1 <- rbind(c1, c[V1 == 2018,])
  c1$VAR <- "s"
  
  
  d <- rbind(a1, b1, c1)
  d$var <- paste0("agg", as.character(i))
  
  e[[i]] <- d
  print(i)
}
######################################
f <- rbindlist(e)

dta2 <- list()
for (i in 1:nrow(f)){
  
  a <- ag[year == unlist(f[i,1]) & var == unlist(f[i,3]) & variable == unlist(f[i,4]),]
  dta2[[i]] <- a
  print(i)
}

dta3 <- rbindlist(dta2)

############### PAPER ###############

dta3 <- dta3[var != "q"] 

##### manuscript plot #####

dta4 <- dta3[yag %in% c(1,5) & year == 2018,]
dta4[var == "p",var:= "Precipitation"]
dta4[var == "s",var:= "Soil moisture"]

dta4[lab == "2018", fct:= factor(x = 2, levels = 2, labels = "2018")]
dta4[lab == "2014-2018", fct:= factor(x = 1, levels = 1, labels = "2014-2018")]

geu = geom_path(aes(x = long, y = lat, group = group), data = eu, size = 0.1)

ggplot(dta4) + 
  geom_tile(aes(x = xc, y = yc, fill = lrnk )) + 
  facet_grid(var ~ fct, switch = "y") + 
  coord_fixed(xlim = xlm, ylim = ylm) + 
  scale_fill_manual(values = c('brown4', 'red3','red', 'orange3', 'orange', 
                               '#6E8B3D', '#666666','grey85', 'white'), 
                    name = 'Exceedence probability', 
                    labels = c("<2", "2-5", "5-10", "10-20", "20-30", "30-40", "40-50", ">50"), 
                    guide = guide_legend(nrow = 1, title.position = 'top',
                                         label.position = "bottom",
                                         keywidth = 3, keyheight = 0.5)) + 
  scale_alpha_discrete(range = c(1, 0), guide = 'none') + 
  geu + 
  theme(panel.background = element_blank(), 
        legend.position = 'bottom',
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        plot.background = element_blank(), strip.background = element_blank()) 

ggsave(filename = "maps_manuscript_001.pdf", device = "pdf", width = 150, units = "mm")

##### supporting info plot #####

geu <- geom_path(aes(x = long, y = lat, group = group), data = eu, size = 0.2)

dta5 <- dta3[yag %in% c(1,5),]
dta5[var == "p",var:= "Precipitation"]
dta5[var == "s",var:= "Soil moisture"]

##### AGG 1 year #####
ggplot(dta5[yag == 1,]) + 
  geom_tile(aes(x = xc, y = yc, fill = lrnk)) + 
  facet_wrap(var ~ lab, ncol = 6) + 
  coord_fixed(xlim = xlm, ylim = ylm) + 
  scale_fill_manual(values = c('brown4', 'red3','red', 'orange3', 'orange', 
                               '#6E8B3D', '#666666','grey85', 'white'), 
                    name = 'Exceedence probability', 
                    labels = c("<2", "2-5", "5-10", "10-20", "20-30", "30-40", "40-50", ">50"), 
                    guide = guide_legend(nrow = 1, title.position = 'top',
                                         label.position = "bottom",
                                         keywidth = 2.6, keyheight = 0.5)) + 
  geu + 
  theme(panel.background = element_blank(), 
        legend.position = 'bottom',
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        plot.background = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = 9), 
        panel.spacing = unit(-2,"mm"), 
        plot.margin = unit(c(-1,-1,-1,-1), "mm")) 

ggsave(filename = "maps_supp_met_001.pdf", device = "pdf", width = 200, units = "mm")

##### AGG 5 year #####
ggplot(dta5[yag == 5,]) + 
  geom_tile(aes(x = xc, y = yc, fill = lrnk )) + 
  facet_wrap(var ~ lab, ncol = 6, labeller = labeller(var = c("Precipitation" = NA, "Soil moisture" = NA))) + 
  coord_fixed(xlim = xlm, ylim = ylm) + 
  scale_fill_manual(values = c('brown4', 'red3','red', 'orange3', 'orange', 
                               '#6E8B3D', '#666666','grey85', 'white'), 
                    name = 'Exceedence probability', 
                    labels = c("<2", "2-5", "5-10", "10-20", "20-30", "30-40", "40-50", ">50"), 
                    guide = guide_legend(nrow = 1, title.position = 'top',
                                         label.position = "bottom",
                                         keywidth = 2.6, keyheight = 0.5)) + 
  geu + 
  theme(panel.background = element_blank(), 
        legend.position = 'bottom',
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        plot.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 9), 
        panel.spacing = unit(-2,"mm"), 
        plot.margin = unit(c(-1,-1,-1,-1), "mm")) 

ggsave(filename = "maps5_supp_met_001.pdf", device = "pdf", width = 200, units = "mm")

########################### end ################################