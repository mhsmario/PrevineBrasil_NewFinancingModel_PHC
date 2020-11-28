#CNES_Variables Part II

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

setwd(here("Previne"))


#1. CNES - N. EAB + N. ESF 2019 ####
CNES_ESF_Equipes <- readRDS(paste0(here("Previne","data","rds"),
                                   "/CNES_ESF_equipes_2019.RDS"))
#Alternative CNES_ESF_unidades_2019_full.RDS

CNES_ESF_Equipes$Portaria_n99_7Fev2020 <- as.character(CNES_ESF_Equipes$Portaria_n99_7Fev2020)
colnames(CNES_ESF_Equipes)[4] <- "Qtd_ESFs" 
#2. CNES - % de UBS com horario de atendimento estendido####
CNES_ESF_HorarioExt <- readRDS(paste0(here("Previne","data","rds"),
                                      "/CNES_ESF_Horario2019.RDS"))
#Alternative CNES_ESF_Horario2019_full.RDS

CNES_ESF_HorarioExt_short <- CNES_ESF_HorarioExt %>%
  group_by(CO_MUNICIPIO_GESTOR, Ano, Mes) %>%
  summarise(Hrs_ate_59hrs = sum(Hrs_ate_59hrs),
            Hrs_60 = sum(Hrs_60),
            Hrs_75 = sum(Hrs_75))

colnames(CNES_ESF_HorarioExt_short)[1] <- "CO_MUNICIPIO"
colnames(CNES_ESF_HorarioExt_short)[4] <- "Qtd_UBS_Hrs_ate_59"
colnames(CNES_ESF_HorarioExt_short)[5] <- "Qtd_UBS_Hrs_60_74"
colnames(CNES_ESF_HorarioExt_short)[5] <- "Qtd_UBS_Hrs_75+"

#3. CNES - numero de profissionais de saude (ASC, medicos, enfermeiras etc)####
CNES_Prof_APS <- readRDS(paste0(here("Previne","data","rds"), "/CNES_Profissionais_APS.RDS"))
#Alternative  CNES_Profissionais_APS_full.RDS

colnames(CNES_Prof_APS)[4] <- "APS_Qtd_Medicos"
colnames(CNES_Prof_APS)[5] <- "APS_Qtd_Enfermeirx"
colnames(CNES_Prof_APS)[6] <- "APS_Qtd_ProfBucal"

#4. CNES - Populaçao Municipio####  
CNES_Municipios <- readRDS((paste0(here("Previne","data","rds"), "/Municipios.RDS")))
CNES_Municipios <- CNES_Municipios[, c(1,2,3,6,7,10,11,12)]

colnames(CNES_Municipios)[7] <- "NU_POPULACAO_CNES"

#Merge
CNES_ESF_Equipes$CO_MUNICIPIO <- as.character(CNES_ESF_Equipes$CO_MUNICIPIO)
CNES_var1 <- left_join(CNES_ESF_Equipes,CNES_ESF_HorarioExt_short, by = c("CO_MUNICIPIO", "Ano", "Mes"))


CNES_Prof_APS$CO_MUNICIPIO <- as.character(CNES_Prof_APS$CO_MUNICIPIO)
CNES_var2 <- left_join(CNES_var1, CNES_Prof_APS, by = c("CO_MUNICIPIO", "Ano", "Mes"))

CNES_Municipios$CO_MUNICIPIO <- as.character(CNES_Municipios$CO_MUNICIPIO)
CNES_var3 <- left_join(CNES_var2, CNES_Municipios, by = c("CO_MUNICIPIO"))

CNES_variables <- CNES_var3[, c(14,1,13,15:19, 3, 5, 6, 4, 7, 8, 9, 10, 11, 12)]

saveRDS(CNES_variables, (paste0(here("Previne","data","rds"), "/CNES_Complete_2019.RDS")))

