#2020####
##Pulling all variables together for 2020

#Inconsistencias:
  #FNS + IBGE = Varias inconsistencias nos nomes dos bairros (ex: de/do/dos, luis/luiz) e o municipio de SANTA TERESINHA aparece como BAHIA no FNS mas no IBGE pertence a PARAIBA.
  #CNES + IBGE = A base do IBGE nao tem Ceilandia DF com um municipio, apenas Brasilia.
  #SISAB + IBGE = Nao consta o número de atendimentos de SP 

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

#install.packages(c("tictoc", "data.table", "readr", "lubridate", "rio"))

setwd(here("Previne"))

#0. Setup ####
rm(list = ls())
gc()

#MERGE Datasets ####

#Fundo Nacional de Saude - Repasses####
  FNS_APS_20 <- readRDS(here("Previne/data/rds/FNS_data_2020_wide.RDS"))
  
  FNS_names <- names(FNS_APS_20)
  FNS_names <- paste0("FNS_",FNS_names)
  colnames(FNS_APS_20) <- FNS_names
  FNS_APS_20_mun <- FNS_APS_20

  ##>>> Changes needed BEFORE merging####
  colnames(FNS_APS_20_mun) [1] <- "UF"
  colnames(FNS_APS_20_mun) [2] <- "NO_MUNICIPIO"
  colnames(FNS_APS_20_mun) [4] <- "Mes"
  FNS_APS_20_mun$Mes <- as.numeric(FNS_APS_20_mun$Mes)
  colnames(FNS_APS_20_mun) [3] <- "Ano"
  FNS_APS_20_mun$Ano <- as.numeric(FNS_APS_20_mun$Ano) 
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("MUQUEM DE SAO FRANCISCO",
                                      "MUQUEM DO SAO FRANCISCO", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("SANTA TERESINHA",
                                      "SANTA TERESINHA", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
  FNS_APS_20_mun$UF <- ifelse(FNS_APS_20_mun$NO_MUNICIPIO == "SANTA TERESINHA",
                              paste0("PB"), FNS_APS_20_mun$UF)
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("ITAPAGE",
                                      "ITAPAJE", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("PASSA-VINTE",
                                      "PASSA VINTE", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("PINGO-D'AGUA",
                                      "PINGO D'AGUA", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("POXOREO",
                                      "POXOREU", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("ELDORADO DOS CARAJAS",
                                      "ELDORADO DO CARAJAS", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("BELEM DE SAO FRANCISCO",
                                      "BELEM DO SAO FRANCISCO", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("OLHO-D'AGUA DO BORGES",
                                      "OLHO D'AGUA DO BORGES", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("BIRITIBA-MIRIM",
                                      "BIRITIBA MIRIM", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("FLORINIA",
                                      "FLORINEA", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  FNS_APS_20_mun$NO_MUNICIPIO <- gsub("SAO LUIS DO PARAITINGA",
                                      "SAO LUIZ DO PARAITINGA", 
                                      FNS_APS_20_mun$NO_MUNICIPIO)
  
#IBGE - PIB Municipal####  
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
    merge1 <- left_join(FNS_APS_20_mun, IBGE, by = c("UF", "NO_MUNICIPIO"))
    
  ### < Check >       #######################
  #Check result
  
  sapply(merge1, function(x) sum(is.na(x)))
  merge1_nas <- merge1 %>% filter(is.na(`IBGE_Amazônia Legal`))
  merge1_nas <- unique(merge1_nas$NO_MUNICIPIO)
  ifelse(length(merge1_nas) == 0, 
         print("Successfully merged all Municipalities!"), 
         print("Oh no, something went wront.") )
  
  rm(FNS_APS_20, FNS_APS_20_mun, IBGE_PIB_Municipios_2017)

  
#CNES ####
  CNES_Complete_2020 <- readRDS(paste0(here("Previne","data","rds"), "/CNES_Complete_2020.RDS"))

  CNES_names <- names(CNES_Complete_2020)
  CNES_names <- paste0("CNES_",CNES_names)
  colnames(CNES_Complete_2020) <- CNES_names
  
  ##>>> Changes needed BEFORE merging####
    #colnames(CNES_Complete_2020) [2] <- "CO_MUNICIPIO"
    colnames(CNES_Complete_2020) [2] <- "CO_MUNICIPIO_m2"
    CNES_Complete_2020$CO_MUNICIPIO_m2 <- as.numeric(CNES_Complete_2020$CO_MUNICIPIO_m2)
    colnames(CNES_Complete_2020) [1] <- "UF"
    colnames(CNES_Complete_2020) [3] <- "NO_MUNICIPIO"
    colnames(CNES_Complete_2020) [10] <- "Mes"
    colnames(CNES_Complete_2020) [11] <- "Ano"  
  
    #MERGE 2####
    merge2 <- left_join(CNES_Complete_2020, IBGE, by = c("UF", "CO_MUNICIPIO_m2"))

    ############################# < >       #######################
    #Check result
    sapply(merge2, function(x) sum(is.na(x)))
    merge1_nas <- merge2 %>% filter(is.na(`IBGE_Amazônia Legal`))
    merge1_nas <- unique(merge1_nas$NO_MUNICIPIO.x)
    ifelse(length(merge1_nas) == 0, 
           print("Successfully merged all Municipalities!"), 
           print(merge1_nas) )
    
    rm(CNES_Complete_2020)
    
#SISAB - Número de consultas/Atendimentos (Relatorio de Produçao)####
        #SISAB - Cobertura por UF##  
          #SISAB_Cobertura_2020 <- read_csv(
          #"data/raw/SISAB/Cobertura/Planilha_Cobertura_2020.csv")
  SISAB_2020 <- readRDS(here("Previne/data/rds/SISAB_2020.RDS"))
  sisab_names <- names(SISAB_2020)
  sisab_names <- paste0("SISAB_",sisab_names)
  colnames(SISAB_2020) <- sisab_names

  ##>>> Changes needed BEFORE merging####
    colnames(SISAB_2020) [1] <- "UF" 
    SISAB_2020$SISAB_Ibge <- ifelse(is.na(SISAB_2020$SISAB_Ibge), paste0("Delete"), SISAB_2020$SISAB_Ibge)  

    SISAB_2020 <- as.data.table(SISAB_2020)
    SISAB_2020 <- SISAB_2020[!SISAB_2020$SISAB_Ibge == "Delete"]
    colnames(SISAB_2020) [2] <- "CO_MUNICIPIO_m2"
    SISAB_2020$CO_MUNICIPIO_m2 <- as.numeric(SISAB_2020$CO_MUNICIPIO_m2)
    colnames(SISAB_2020) [3] <- "NO_MUNICIPIO"
    colnames(SISAB_2020) [10] <- "Mes"
    SISAB_2020$Mes <- as.numeric(SISAB_2020$Mes)
    colnames(SISAB_2020) [11] <- "Ano"
    SISAB_2020$Ano <- as.numeric(SISAB_2020$Ano)

  #MERGE 3 ####
    merge3 <- left_join(SISAB_2020, IBGE, by = c("UF", "CO_MUNICIPIO_m2"))
  
    ############################# < >       #######################
    #Check result
    sapply(merge3, function(x) sum(is.na(x)))
    merge1_nas <- merge3 %>% filter(is.na(`SISAB_Atendimento Individual`))
    merge1_nas <- unique(merge1_nas$NO_MUNICIPIO.x)
    ifelse(length(merge1_nas) == 0, 
           print("Successfully merged all Municipalities!"), 
           print(merge1_nas) )
    
    rm(SISAB_2020)
  
#SIH - Preventable diseases and deaths####  
    SIH_AIH_ACSC_2020 <- readRDS(paste0(here("Previne","data","rds"),
                                            "/SIH_AIH_ACSC_2020_totals.RDS"))
    
    sih_names <- names(SIH_AIH_ACSC_2020)
    sih_names2 <- paste0("SIH_",sih_names)
    colnames(SIH_AIH_ACSC_2020) <- sih_names2
    #SIH_AIH_ACSC_2019 <- readRDS(paste0(here("Previne","data","rds"),
    #                                     "/SIH_AIH_ACSC_2019_totals.RDS"))
    ##>>> Changes needed BEFORE merging####                                     
    colnames(SIH_AIH_ACSC_2020) [1] <- "CO.UF"
    colnames(SIH_AIH_ACSC_2020) [4] <- "CO_MUNICIPIO_m2"
    SIH_AIH_ACSC_2020$CO_MUNICIPIO_m2 <- as.numeric(
      SIH_AIH_ACSC_2020$CO_MUNICIPIO_m2)
    colnames(SIH_AIH_ACSC_2020) [3] <- "Mes"
    SIH_AIH_ACSC_2020$Mes <- as.numeric(SIH_AIH_ACSC_2020$Mes)
    colnames(SIH_AIH_ACSC_2020) [2] <- "Ano"
    SIH_AIH_ACSC_2020$Ano <- as.numeric(SIH_AIH_ACSC_2020$Ano)
    #MERGE 4 ####
    merge4 <- left_join(SIH_AIH_ACSC_2020, IBGE, by = c("CO_MUNICIPIO_m2"))
  
  ############################# < >       #######################
    #Check result
    
    sapply(merge4, function(x) sum(is.na(x)))
    
    rm(SIH_AIH_ACSC_2020)
  
#COVID-19####  
  COVID19_31.08 <- readRDS(paste0(here("Previne","data","rds"), "/COVID_Aug31.RDS"))
    ##>>> Changes needed BEFORE merging####
  COVID19_31.08 <- as.data.table(COVID19_31.08)  
  COVID19_31.08 <- COVID19_31.08[!codmun == ""]
  COVID19_31.08 <- COVID19_31.08[!estado == ""]
  COVID19_31.08 <- COVID19_31.08[!municipio == ""]
  COVID_PIB_names <- names(COVID19_31.08)
  COVID_PIB_names <- paste0("COVID_",COVID_PIB_names)
  colnames(COVID19_31.08) <- COVID_PIB_names
   colnames(COVID19_31.08) [4] <- "CO.UF"
  colnames(COVID19_31.08) [5] <- "CO_MUNICIPIO_m2"
  COVID19_31.08$CO_MUNICIPIO_m2 <- as.numeric(COVID19_31.08$CO_MUNICIPIO_m2)
  colnames(COVID19_31.08) [10] <- "Mes"
  COVID19_31.08$Mes <- as.numeric(COVID19_31.08$Mes)
  COVID19_31.08$Ano <- "2020"
  COVID19_31.08$Ano <- as.numeric(COVID19_31.08$Ano)
  
  #MERGE 5 ####
  merge5 <- left_join(COVID19_31.08, IBGE, by = c("CO_MUNICIPIO_m2"))
  
  ############################# < >       #######################
  #Check result
  
  sapply(merge5, function(x) sum(is.na(x)))
  
  rm(COVID19_31.08)
  rm(merge1_nas)


#FINAL MERGE####
  
  final_merge2 <- merge2[, c(1:18,24)]
  final_merge3 <- merge3[, c(1:7,10,11,17)]
  final_merge4 <- merge4[, c(1:12,19)]
  final_merge5 <- merge5[, c(1:22,29)]
 
  final_1 <- dplyr::left_join(merge1, final_merge2, by = c("CO_MUNICIPIO", "Mes", "Ano"))  
  sapply(final_1, function(x) sum(is.na(x)))
  
  final_2 <- left_join(final_1, final_merge3, by = c("CO_MUNICIPIO", "Mes", "Ano"))  
  final_3 <- left_join(final_2, final_merge4, by = c("CO_MUNICIPIO", "Mes", "Ano")) 
  final_4 <- left_join(final_3, final_merge5, by = c("CO_MUNICIPIO", "Mes", "Ano")) 
  
  rm(IBGE, merge1, merge2, merge3, merge4, merge5)
  
  final_names2 <- as.data.frame(names(final_4))
  
  Previne_dataset <-  final_4[,c(19,52,20,21,73,2,4,3,17,18,26,27,28,29,30,31,32,33,34,5,6,7,8,9,10,11,12,13,14,15,39,40,41,43,44,45,46,47,48,49,50,51,55,56,57,58,61,62,63,64,65,66,67,68,75,76,77,78,79,80,81,83,84,88)]
  
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
  saveRDS(Previne_dataset_pov, paste0(here("Previne/data/rds/Previne_dataset_2020.RDS")))
  #Previne_dataset <- readRDS(paste0(here("Previne/data/rds/Previne_dataset_2020.RDS")))
  
  #Export to DTA
  
  namesdf <- paste0("Var", 1:length(Previne_dataset_pov))
  
  colnames(Previne_dataset_pov) <- namesdf
  
  export(Previne_dataset_pov, "Previne_dataset_2020.dta")
  
  
  