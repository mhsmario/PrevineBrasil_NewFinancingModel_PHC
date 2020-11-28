#2019#####
##Pulling all variables together for 2019

#Inconsistencias:
#FNS + IBGE = .
#CNES + IBGE = .
#SISAB + IBGE =  

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
library(rio)
library(readxl)

setwd(here("Previne"))

#0. Setup ####
rm(list = ls())
gc()

#MERGE Datasets ####

#1. Fundo Nacional de Saude - Repasses####
FNS_APS_19 <- readRDS(here("Previne/data/rds/FNS_data_2019_wide.RDS"))

FNS_names <- names(FNS_APS_19)
FNS_names <- paste0("FNS_",FNS_names)
colnames(FNS_APS_19) <- FNS_names
FNS_APS_19_mun <- FNS_APS_19

##>>> Changes needed BEFORE merging####
colnames(FNS_APS_19_mun) [1] <- "UF"
colnames(FNS_APS_19_mun) [2] <- "NO_MUNICIPIO"
colnames(FNS_APS_19_mun) [4] <- "Mes"
FNS_APS_19_mun$Mes <- as.numeric(FNS_APS_19_mun$Mes)
colnames(FNS_APS_19_mun) [3] <- "Ano"
FNS_APS_19_mun$Ano <- as.numeric(FNS_APS_19_mun$Ano) 
FNS_APS_19_mun$NO_MUNICIPIO <- gsub("MUQUEM DE SAO FRANCISCO",
                                    "MUQUEM DO SAO FRANCISCO", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)
FNS_APS_19_mun$NO_MUNICIPIO <- gsub("SANTA TERESINHA",
                                    "SANTA TERESINHA", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

FNS_APS_19_mun$UF <- ifelse(FNS_APS_19_mun$NO_MUNICIPIO == "SANTA TERESINHA",
                            paste0("PB"), FNS_APS_19_mun$UF)
FNS_APS_19_mun$NO_MUNICIPIO <- gsub("ITAPAGE",
                                    "ITAPAJE", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

FNS_APS_19_mun$NO_MUNICIPIO <- gsub("PASSA-VINTE",
                                    "PASSA VINTE", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

FNS_APS_19_mun$NO_MUNICIPIO <- gsub("PINGO-D'AGUA",
                                    "PINGO D'AGUA", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)
FNS_APS_19_mun$NO_MUNICIPIO <- gsub("POXOREO",
                                    "POXOREU", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

FNS_APS_19_mun$NO_MUNICIPIO <- gsub("ELDORADO DOS CARAJAS",
                                    "ELDORADO DO CARAJAS", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

FNS_APS_19_mun$NO_MUNICIPIO <- gsub("BELEM DE SAO FRANCISCO",
                                    "BELEM DO SAO FRANCISCO", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

FNS_APS_19_mun$NO_MUNICIPIO <- gsub("OLHO-D'AGUA DO BORGES",
                                    "OLHO D'AGUA DO BORGES", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

FNS_APS_19_mun$NO_MUNICIPIO <- gsub("BIRITIBA-MIRIM",
                                    "BIRITIBA MIRIM", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)
FNS_APS_19_mun$NO_MUNICIPIO <- gsub("FLORINIA",
                                    "FLORINEA", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)
FNS_APS_19_mun$NO_MUNICIPIO <- gsub("SAO LUIS DO PARAITINGA",
                                    "SAO LUIZ DO PARAITINGA", 
                                    FNS_APS_19_mun$NO_MUNICIPIO)

#2. IBGE - PIB Municipal####  
IBGE_PIB_Municipios_2017 <- read_csv(here("Previne/data/raw/IBGE/IBGE_PIB_Municipios_2017.csv"))

IBGE <- IBGE_PIB_Municipios_2017[,c(1:9,20,21,22,25,27:31,39, 40, 41)]

IBGE_PIB_names <- names(IBGE)
IBGE_PIB_names <- paste0("IBGE_",IBGE_PIB_names)
colnames(IBGE) <- IBGE_PIB_names

##>>> Changes needed BEFORE merging####
colnames(IBGE) [4] <- "CO.UF"
colnames(IBGE) [5] <- "UF"
colnames(IBGE) [7] <- "CO_MUNICIPIO"
IBGE$CO_MUNICIPIO <- as.numeric(IBGE$CO_MUNICIPIO)
colnames(IBGE) [8] <- "NO_MUNICIPIO"
IBGE$CO_MUNICIPIO_m2 <- gsub('.{1}$', '', IBGE$CO_MUNICIPIO)
IBGE$CO_MUNICIPIO_m2 <- as.numeric(IBGE$CO_MUNICIPIO_m2)
IBGE <- as.data.table(IBGE)
IBGE[, NO_MUNICIPIO := stri_trans_general(str = NO_MUNICIPIO, 
                                          id = "Latin-ASCII")]
IBGE$NO_MUNICIPIO <- toupper(IBGE$NO_MUNICIPIO)
IBGE$NO_MUNICIPIO <- gsub("SANTA TERESINHA", "SANTA TERESINHA",  
                          IBGE$NO_MUNICIPIO)

###MERGE 1 ####
merge1 <- left_join(FNS_APS_19_mun, IBGE, by = c("UF", "NO_MUNICIPIO"))

### < Check >       #######################
#Check result
sapply(merge1, function(x) sum(is.na(x)))
merge1_nas <- merge1 %>% filter(is.na(`IBGE_Amazônia Legal`))
merge1_nas <- unique(merge1_nas$NO_MUNICIPIO)
ifelse(length(merge1_nas) == 0, 
       print("Successfully merged all Municipalities!"), 
       print("Oh no, something went wront.") )

rm(FNS_APS_19, FNS_APS_19_mun)
rm(IBGE_PIB_Municipios_2017)


#3. CNES ####
CNES_Complete_2019 <- readRDS(paste0(here("Previne","data","rds"), "/CNES_Complete_2019.RDS"))

CNES_names <- names(CNES_Complete_2019)
CNES_names <- paste0("CNES_",CNES_names)
colnames(CNES_Complete_2019) <- CNES_names

##>>> Changes needed BEFORE merging####
#colnames(CNES_Complete_2019) [2] <- "CO_MUNICIPIO"
colnames(CNES_Complete_2019) [2] <- "CO_MUNICIPIO_m2"
CNES_Complete_2019$CO_MUNICIPIO_m2 <- as.numeric(CNES_Complete_2019$CO_MUNICIPIO_m2)
colnames(CNES_Complete_2019) [1] <- "UF"
colnames(CNES_Complete_2019) [3] <- "NO_MUNICIPIO"
colnames(CNES_Complete_2019) [10] <- "Mes"
colnames(CNES_Complete_2019) [11] <- "Ano"  

#MERGE 2####
merge2 <- left_join(CNES_Complete_2019, IBGE, by = c("UF", "CO_MUNICIPIO_m2"))


############################# < >       #######################
#Check result
sapply(merge2, function(x) sum(is.na(x)))
merge1_nas <- merge2 %>% filter(is.na(`IBGE_Amazônia Legal`))
merge1_nas <- unique(merge1_nas$NO_MUNICIPIO.x)
ifelse(length(merge1_nas) == 0, 
       print("Successfully merged all Municipalities!"), 
       print(merge1_nas) )

rm(CNES_Complete_2019)

#4. SISAB - Número de consultas/Atendimentos (Relatorio de Produçao)####
#SISAB - Cobertura por UF##  
#SISAB_Cobertura_2019 <- read_csv(
#"data/raw/SISAB/Cobertura/Planilha_Cobertura_2019.csv")
SISAB_2019 <- readRDS(here("Previne/data/rds/SISAB_2019.RDS"))
sisab_names <- names(SISAB_2019)
sisab_names <- paste0("SISAB_",sisab_names)
colnames(SISAB_2019) <- sisab_names

##>>> Changes needed BEFORE merging####
colnames(SISAB_2019) [1] <- "UF" 
SISAB_2019$SISAB_Ibge <- ifelse(is.na(SISAB_2019$SISAB_Ibge), paste0("Delete"), SISAB_2019$SISAB_Ibge)  

SISAB_2019 <- as.data.table(SISAB_2019)
SISAB_2019 <- SISAB_2019[!SISAB_2019$SISAB_Ibge == "Delete"]
colnames(SISAB_2019) [2] <- "CO_MUNICIPIO_m2"
SISAB_2019$CO_MUNICIPIO_m2 <- as.numeric(SISAB_2019$CO_MUNICIPIO_m2)
colnames(SISAB_2019) [3] <- "NO_MUNICIPIO"
colnames(SISAB_2019) [10] <- "Mes"
SISAB_2019$Mes <- as.numeric(SISAB_2019$Mes)
colnames(SISAB_2019) [11] <- "Ano"
SISAB_2019$Ano <- as.numeric(SISAB_2019$Ano)

#MERGE 3 ####
merge3 <- left_join(SISAB_2019, IBGE, by = c("UF", "CO_MUNICIPIO_m2"))

############################# < >       #######################
#Check result
sapply(merge3, function(x) sum(is.na(x)))
merge1_nas <- merge3 %>% filter(is.na(`SISAB_Atendimento Individual`))
merge1_nas <- unique(merge1_nas$NO_MUNICIPIO)
ifelse(length(merge1_nas) == 0, 
       print("Successfully merged all Municipalities!"), 
       print(merge1_nas) )

rm(SISAB_2019)

#5. SIH - Preventable diseases and deaths####  
SIH_AIH_ACSC_2019 <- readRDS(paste0(here("Previne","data","rds"),
                                    "/SIH_AIH_ACSC_2019_totals.RDS"))

sih_names <- names(SIH_AIH_ACSC_2019)
sih_names2 <- paste0("SIH_",sih_names)
colnames(SIH_AIH_ACSC_2019) <- sih_names2
#SIH_AIH_ACSC_2019 <- readRDS(paste0(here("Previne","data","rds"),
#                                     "/SIH_AIH_ACSC_2019_totals.RDS"))
##>>> Changes needed BEFORE merging####                                     
colnames(SIH_AIH_ACSC_2019) [1] <- "CO.UF"
colnames(SIH_AIH_ACSC_2019) [4] <- "CO_MUNICIPIO_m2"
SIH_AIH_ACSC_2019$CO_MUNICIPIO_m2 <- as.numeric(
  SIH_AIH_ACSC_2019$CO_MUNICIPIO_m2)
colnames(SIH_AIH_ACSC_2019) [3] <- "Mes"
SIH_AIH_ACSC_2019$Mes <- as.numeric(SIH_AIH_ACSC_2019$Mes)
colnames(SIH_AIH_ACSC_2019) [2] <- "Ano"
SIH_AIH_ACSC_2019$Ano <- as.numeric(SIH_AIH_ACSC_2019$Ano)
#MERGE 4 ####
merge4 <- left_join(SIH_AIH_ACSC_2019, IBGE, by = c("CO_MUNICIPIO_m2"))

############################# < >       #######################
#Check result

sapply(merge4, function(x) sum(is.na(x)))

rm(SIH_AIH_ACSC_2019)

#6. FINAL MERGE####

final_merge2 <- merge2[, c(1:18,24)]
final_merge3 <- merge3[, c(1:7,10,11,17)]
final_merge4 <- merge4[, c(1:12,19)]

final_1 <- dplyr::left_join(merge1, final_merge2, by = c("CO_MUNICIPIO", "Mes", "Ano"))  
sapply(final_1, function(x) sum(is.na(x)))

final_2 <- left_join(final_1, final_merge3, by = c("CO_MUNICIPIO", "Mes", "Ano"))  
final_3 <- left_join(final_2, final_merge4, by = c("CO_MUNICIPIO", "Mes", "Ano")) 

rm(IBGE, merge1, merge2, merge3, merge4)

final_names2 <- as.data.frame(names(final_3))

Previne_dataset <-  final_3[,c(21,54,22,23,37,2,3,4,19,28,29,30,31,32,33,34,35,36,5,20,7,8,9,10,6,11,12,14,15,13,17,16,41,42,43,45,46,47,48,49,50,51,52,53,57,58,59,60,63,64,65,66,67,68,69,70)]

rm(final_merge2, final_merge3, final_merge4, final_merge5)
rm(final_1, final_2, final_3, final_4)

final_names <- as.data.frame(names(Previne_dataset))

rm(final_names, final_names2)

#IBGE - Linhas de Pobreza####   
IBGE_Poverty <- read_xlsx(here("Previne/data/raw/IBGE/Poverty Censo.xlsx"),
                          skip = 2) 

IBGE_Poverty <- IBGE_Poverty[,-c(2,3,8:13)]

##>>> Changes needed BEFORE merging####
colnames(IBGE_Poverty) [1] <- "CO_MUNICIPIO"
IBGE_Poverty$CO_MUNICIPIO <- as.numeric(IBGE_Poverty$CO_MUNICIPIO)

Previne_dataset_pov <- left_join(Previne_dataset, IBGE_Poverty, by = "CO_MUNICIPIO")

##Save RDS####
#Previne_dataset <- Previne_dataset[,-c(48,60,61,26,10,11,12)]
saveRDS(Previne_dataset_pov, paste0(here("Previne/data/rds/Previne_dataset_2019.RDS")))
#Previne_dataset <- readRDS(paste0(here("Previne/data/rds/Previne_dataset_2019.RDS")))

namesdf <- paste0("Var", 1:60)

colnames(Previne_dataset_pov) <- namesdf

export(Previne_dataset_pov, paste0(here("Previne/data/Previne_dataset_2019.dta")))


