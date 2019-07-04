source("./source/libs.R")

eu = readRDS('./data/geo/Europe.rds')
geu = geom_path(aes(x = long, y = lat, group = group), data = eu, size = 0.5)
xlm = bbox(eu)['x', ]
ylm = bbox(eu)['y',]

#dta <- readRDS('./data/dta/rs_met_001.rds')
dta <- readRDS('./data/dta/rs_met_002.rds')

dta[,PER:= cut(year(DTM), 
                   breaks = seq(1751,2050,30), 
                   labels =c("1751-1780","1781-1810","1811-1840","1841-1870",
                             "1871-1900","1901-1930","1931-1960","1961-1990",
                             "1991-2020"))]

dta[, SXI1 := qnorm((frank(value, na.last = 'keep') - .3)/(.N + 0.4)), by = .(xc, yc, LON, LAT, REG, var, month(DTM), PER)]
dta[, SXI3 := qnorm((frank(rollmean(value, k = 3, fill = NA, align = 'right'), na.last = 'keep') - .3)/(.N + 0.4)), by = .(xc, yc, LON, LAT, REG, var, month(DTM), PER)]
#dta[, SXI6 := qnorm((frank(rollmean(value, k = 6, fill = NA, align = 'right'), na.last = 'keep') - .3)/(.N + 0.4)), by = .(xc, yc, LON, LAT, REG, var)]

dta[, SXI := ifelse(var != 'p', SXI1, SXI3)]

dta <- dta[var %in% c('t', 'pet'), .(xc, yc, LON, LAT, REG, DTM, var, SXI)]
gc()

dta[, incl := SXI > qnorm(0.8)]

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
#ag <- ag[year > 1899]

ag[yag != 1, lab := paste0(year - yag + 1, '-', year)]
ag[yag == 1, lab := as.character(year)]
#ag[, rnk := frank(agval, na.last = 'keep'), by = .(xc, yc, LON, LAT, REG, var, variable, yag)]
ag[, rnk := frankv(x = agval, order = -1, na.last = 'keep'), by = .(xc, yc, LON, LAT, REG, var, variable, yag)]
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

e <- list()
for (i in unique(ag$variable)){
  a <- data.table(ag[variable == i, year[which(p < 0.05)], by = .(var, LON, LAT)][var=='pet', table(V1)])
  a1 <- a[V1 != 2018,][N %in% sort(N, decreasing = T)[1:5],]
  a1 <- rbind(a1, a[V1 == 2018,])
  a1$VAR <- "pet"
  
  b <- data.table(ag[variable == i, year[which(p < 0.05)], by = .(var, LON, LAT)][var=='t', table(V1)])
  b1 <- b[V1 != 2018,][N %in% sort(N, decreasing = T, )[1:5],]
  b1 <- rbind(b1, b[V1 == 2018,])
  b1$VAR <- "t"
  
  #c <- data.table(ag[variable == i, year[which(p < 0.05)], by = .(var, LON, LAT)][var=='s', table(V1)])
  #c1 <- c[V1 != 2018,][N %in% sort(N, decreasing = T)[1:5],]
  #c1 <- rbind(c1, c[V1 == 2018,])
  #c1$VAR <- "s"
  
  
  d <- rbind(a1, b1)
  d$var <- i
  
  e[[i]] <- d
  print(i)
}

f <- do.call(rbind, e)

dta2 <- list()
for (i in 1:nrow(f)){
  
  a <- ag[year == unlist(f[i,1]) & var == unlist(f[i,3]) & variable == unlist(f[i,4]),]
  dta2[[i]] <- a
  print(i)
}

dta3 <- do.call(rbind, dta2)

for (i in 1:10){
  
  ggplot(dta3[yag == i,]) + 
    geom_tile(aes(x = xc, y = yc, alpha = lrnk, fill = lrnk )) + 
    facet_wrap(var ~ lab, ncol = 6) + 
    coord_fixed(xlim = xlm, ylim = ylm) + 
    scale_fill_manual(values = c('brown4', 'red3','red', 'orange3', 'orange', 
                                 'deepskyblue4', 'skyblue','gray', 'white'), 
                      name = 'RANK', guide = guide_legend(nrow = 1, title.position = 'top')) + 
    scale_alpha_discrete(range = c(1, 0), guide = 'none') + 
    theme_void() + geu + theme(legend.position = 'bottom') 
  
  ggsave(filename = paste0("MAP_agg_", i, ".png"), device = "png", path = "./results/", width = 297, height = 210, units = "mm", dpi = 300)
}
