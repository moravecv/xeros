library(data.table)
library(SPEI)
library(magrittr)
library(zoo)
library(ggplot2)
library(foreign)
library(dplyr)
library(forcats)
library(grid)
library(gridExtra)
library(ggrepel)
library(viridis)

###############################

create_labs <- function(n){
  a <- 2018 - (ceiling(253/n) * n) 
  b <- 2018
  sq <- seq(from = a, to = b, by = n)
  
  labs <- c()
  for (i in 1:(length(sq)-1)){
    l <- paste0(sq[i] + 1, "-", sq[i] + n)
    labs <- append(labs, l)
  }
  res <- list(labs, a, sq)
  res
}

pp <- function(p, N){((p - 0.3)/(N + 0.4))}
