#Fundo Nacional da Saúde - Repasse da Atençao Basica (PREVINE) aos municipios.
#Ano: 2020, 2019, 2018

#0. Setup####
library(here)
library(tictoc)
library(dplyr)
library(stringi)
library(data.table)
library(xlsx)
library(lubridate)

rm(list = ls())

#1. Load RDS ####
#The data was downloaded via webscrapping the Json pages

FNS_APS_181920 <- readRDS(paste0(here("Previne","data","rds"), "/FNS_APS_data.RDS"))
FNS_rest <- readRDS(paste0(here("Previne","data","rds"), "/FNS_APS_data_rest.RDS"))
FNS_rest2 <- readRDS(paste0(here("Previne","data","rds"), "/FNS_APS_data_rest2.RDS"))

FNS_rest3 <- rbind(FNS_rest2, FNS_rest)
FNS_rest3 <- unique(FNS_rest3)
rm(FNS_rest2, FNS_rest)

FNS_APS_181920_Final <- rbind(FNS_APS_181920, FNS_rest3)
rm(FNS_APS_181920)
##Keep uniques (aka remove duplicates)
FNS_APS_181920_Final <- unique(FNS_APS_181920_Final) 

##2. Fix Compentencia####

FNS_APS_181920_Final$dsCompetencia <- gsub(" em ", "/", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub(" de ", "/", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("JAN", "1", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("FEV", "2", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("MAR", "3", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("ABR", "4", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("MAI", "5", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("JUN", "6", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("JUL", "7", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("AGO", "8", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("SET", "9", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("OUT", "10", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("NOV", "11", FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- gsub("DEZ", "12", FNS_APS_181920_Final$dsCompetencia)

FNS_APS_181920_Final$dsCompetencia <- ifelse(nchar(FNS_APS_181920_Final$dsCompetencia) < 7, paste0("01/", FNS_APS_181920_Final$dsCompetencia), FNS_APS_181920_Final$dsCompetencia)
FNS_APS_181920_Final$dsCompetencia <- FNS_APS_181920_Final$dsCompetencia


FNS_APS_20 <- FNS_APS_181920_Final[FNS_APS_181920_Final$nuAno == 2020,]
FNS_APS_19 <- FNS_APS_181920_Final[FNS_APS_181920_Final$nuAno == 2019,]

#4. Save ####
saveRDS(FNS_APS_181920_Final, file = here("Previne/data/rds/FNS_APS_181920_Final_OCT_02_2020.RDS"))
saveRDS(FNS_APS_20, file = here("Previne/data/rds/FNS_APS_20.RDS"))

write.csv(fns_previne_jan_ago_2019, file = here("Previne/data/FNS_APS_Previne_janAago_2019.csv"))
