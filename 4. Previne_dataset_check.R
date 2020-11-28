#Previne Dataset


#Libraries####
library(dplyr)
library(here)
library(tidyr)
library(stringr)
library(stringi)
library(tictoc)
library(data.table)
library(purrr)
library(readr)
library(lubridate)
library(ggplot2)
library(VIM)

#setwd(here("Previne"))

#0. Setup ####
rm(list = ls())
gc()

Previne_dataset_check_19 <- readRDS((here("Previne/data/rds/Previne_dataset_2019.RDS")))
Previne_dataset_check_20 <- readRDS((here("Previne/data/rds/Previne_dataset_2020.RDS")))
Previne_dataset <- Previne_dataset_check_19
Previne_dataset <- Previne_dataset_check_20
# Previne_dataset1 <- Previne_dataset[, c(1,2,3:30)]
# Previne_dataset2 <- Previne_dataset[, c(1,2,31:62)]

#Investigate NAs and patterns
prev_nas <- as.data.frame(colSums(is.na(Previne_dataset)))

Previne_dataset <- as.data.table(Previne_dataset)
Previne_dataset[, .(sumNA = sum(is.na(.SD))), by = Mes]

nas <- Previne_dataset %>% 
  group_by(Mes) %>% 
  summarise_each(funs(sum(is.na(.))))

nas2 <- as.data.frame(t(nas))

# prev_nas2 <- Previne_dataset %>%
#   group_by(Mes, Ano) %>%
#   summarise(NAs = sum(is.na))

colnames(prev_nas)[1] <- "N.nas"
prev_nas$pct <- prev_nas$N.nas/nrow(Previne_dataset)
prev_nas$obs <- nrow(Previne_dataset) - prev_nas$N.nas


#Other things
aggr(Previne_dataset1, combined = F ,numbers = TRUE,
     cex.lab = .3,
     cex.axis = .3) 

aggr(Previne_dataset2, numbers = T, prop = c(TRUE, FALSE),
     cex.lab = .4,
     cex.axis = .4) 
  
prev_nas_50 <- prev_nas[prev_nas$pct > .49,]

Previne_dataset <- Previne_dataset[,-c(48,60,61,26,10,11,12)]

saveRDS(Previne_dataset, paste0(here("Previne/data/rds/Previne_dataset_2020.RDS")))
