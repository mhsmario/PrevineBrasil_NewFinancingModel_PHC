#CNES Variables

#Section 1
#VAR:    O número de Equipes da Atenção Básica (EAB)
#VAR:    O número de Equipes da ESF
#SOURCE: "tbEquipe201908"
#Output file =  ESF_equipes_2019.RDS

#Section 2
#VAR:    População do Municipio
#SOURCE: tbMunicipio201908.csv
#Output file =  Municipios.RDS

#Section 3
#VAR:    Percentual de UBS que adotaram horario de atendimento estendido
#SOURCE: tbEstabelicimentos2019XX.csv + tbEstabelicimentosHorario2019XX.csv
#Output file =  ESF_Horario2019.RDS
#Output file =  ESF_Horario2019_full.RDS
#Output file =  CNES_EstabProgFundo_APS_Unique_2019.RDS 
#Output file =  CNES_Estabelecimentos_APS_2019.RDS

#Section 4
#VAR:    numero de profissionais de saude (ASC, medicos, enfermeiras etc) 
#SOURSE:  + RL_ESTAB_EQUIPE_PROF2019XX.csv
#SOURCE: https://integracao.esusab.ufsc.br/v211/docs/cbo.html
#Output file =    CNES_Profissionais_APS_full.RDS
#Output file =    CNES_Profissionais_APS.RDS

#VAR:    numero de leitos e hospitais por n. de leitos (ver com Stella Lobo)
#SOURCE: { CNES} RL_ESTAB_COMPLEMENTAR2019XX.csv + leitos

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

#0. Setup ####
rm(list = ls())


read_plus <- function(flnm) {
  fread(flnm) %>% 
    mutate(filename = flnm)
}


All_CNES_Tables <- as.data.frame(list.files(path = here("Previne","data", "raw", "CNES_bases", "2019"), 
                                            pattern = ".csv",recursive = T))

#1. VAR:    N. EAB + N. ESF####
#SOURCE: tbEstabelecimento2 tbEquipe2019..

tbEquipe2019_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "tbEquipe2019..*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))


##Adjust year and month
tbEquipe2019_raw <- separate(tbEquipe2019_raw, filename, into = c("filepath", "Ano.Mes"), sep = "tbEquipe")

tbEquipe2019_raw$Ano.Mes <- gsub(".csv", "/01", tbEquipe2019_raw$Ano.Mes)
tbEquipe2019_raw$Ano.Mes <- gsub("2019", "2019/", tbEquipe2019_raw$Ano.Mes)

##Summarise
Equipes_short <- tbEquipe2019_raw %>% 
  group_by(CO_MUNICIPIO,CO_UNIDADE, TP_EQUIPE, Ano.Mes) %>% 
  summarise(count = n())

portaria99_ajuste_tipologia_equipes <- read.csv("~/Previne/data/portaria99_ajuste_tipologia_equipes.csv")

colnames(portaria99_ajuste_tipologia_equipes)[1] <- "TP_EQUIPE"
colnames(portaria99_ajuste_tipologia_equipes)[4] <- "NEW_TP_EQUIPE"

Equipes_2019 <- left_join(Equipes_short, portaria99_ajuste_tipologia_equipes, by = c("TP_EQUIPE") )

ESF_Equipes_2019 <- Equipes_2019 %>%
  filter(NEW_TP_EQUIPE == 70) %>% #ESF teams only
  group_by(CO_MUNICIPIO, CO_UNIDADE, NEW_TP_EQUIPE,Portaria_n99_7Fev2020, Ano.Mes) %>%
  summarise(count = n())

ESF_Unidades <-   as.data.frame(c(ESF_Equipes_2019$CO_UNIDADE, ESF_Equipes_2019$CO_MUNICIPIO))

CNES_ESF_Equipes_short <- ESF_Equipes_2019 %>%
  group_by(CO_MUNICIPIO, Portaria_n99_7Fev2020, Ano.Mes) %>%
  summarise(Qtd = sum(count))

CNES_ESF_Equipes_short$Ano.Mes <- ymd(CNES_ESF_Equipes_short$Ano.Mes)
CNES_ESF_Equipes_short$Mes <- month(CNES_ESF_Equipes_short$Ano.Mes)
CNES_ESF_Equipes_short$Ano <- year(CNES_ESF_Equipes_short$Ano.Mes)

#Save.RDS####
saveRDS(ESF_Equipes_2019, paste0(here("Previne","data","rds"), "/CNES_ESF_equipes_2019_full.RDS"))
saveRDS(CNES_ESF_Equipes_short, paste0(here("Previne","data","rds"), "/CNES_ESF_equipes_2019.RDS"))

saveRDS(ESF_Unidades, paste0(here("Previne","data","rds"), "/CNES_ESF_unidades_2019.RDS"))

#2. VAR:    Populaçao Municipio####  

tbMun2019_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "tbMunicipio201901.csv?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))
#Save.RDS####  
saveRDS(tbMun2019_raw, paste0(here("Previne","data","rds"), "/Municipios.RDS"))


#3. VAR:    % de UBS com horario de atendimento estendido####
#SOURCE: tbEstabelicimentos2019XX.csv + tbEstabelicimentosHorario2019XX.csv

#3.0 Nivel de atençao dos estabelecimentos####
rlEstabProgFundo201908 <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "rlEstabProgFundo2019..*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))

#Tipo de atençao
tbGestao2019_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "tbGestao2019..*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))
tbGestao2019 <- tbGestao2019_raw %>%
  group_by(CO_GESTAO,DS_GESTAO,TP_PROG) %>%
  summarise(n = n())
tbGestao2019 <- tbGestao2019[, -4]
rm(tbGestao2019_raw)

colnames(tbGestao2019)[1] <- "CO_ATIVIDADE"

rlEstabProgFundo_Gestao <- left_join(rlEstabProgFundo201908, tbGestao2019, 
                                     by = "CO_ATIVIDADE" )

rm(rlEstabProgFundo201908)

rlEstabProgFundo_Gestao_unique <- rlEstabProgFundo_Gestao %>%
  filter(CO_ATIVIDADE == 1) %>%
  group_by(CO_UNIDADE, CO_ATIVIDADE, DS_GESTAO) %>%
  summarise(n = n())

rm(rlEstabProgFundo_Gestao)
rm(tbGestao2019)
gc()

#3.1 Estabelecimentos####
#Special function to call and filter estabelecimentos da APS
read_plus_filter <- function(flnm) {
  fread(flnm, colClasses = 'character') %>% 
    mutate(filename = flnm) %>%
    filter(CO_UNIDADE %in% rlEstabProgFundo_Gestao_unique$CO_UNIDADE)
}


tbEstabelecimentos_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "tbEstabelecimento2019..*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus_filter(.))


tbEstabelecimentos <- tbEstabelecimentos_raw[, c(1,2,12,22,23,29, 30, 32, 40, 41, 50, 52, 53)]

rm(tbEstabelecimentos_raw)
gc()


#Save.RDS####  

saveRDS(rlEstabProgFundo_Gestao_unique, paste0(here("Previne","data","rds"),
                                               "/CNES_EstabProgFundo_APS_Unique_2019.RDS"))

saveRDS(tbEstabelecimentos, paste0(here("Previne","data","rds"), "/CNES_Estabelecimentos_APS_2019.RDS"))


#Filtre por unidades relevantes
ESF_unidades_2019 <- readRDS(paste0(here("Previne","data","rds"), "/CNES_ESF_equipes_2019_full.RDS"))
ESF_unidades_2019_unique <- as.data.frame(unique(ESF_unidades_2019$CO_UNIDADE))
rm(ESF_unidades_2019)
ESF_unidades_2019_unique$x <- as.character(ESF_unidades_2019_unique$x) 

#Filtre por ESFs apenas
ESF_unidades_ESTA_2019 <- subset(tbEstabelecimentos, CO_UNIDADE %in% ESF_unidades_2019_unique$x)

#Tipos de Estabelecimentos
tbTIPOEstab_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "tbTipoEstabelecimento201908*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))

tbTIPOEstab <- unique(tbTIPOEstab_raw[,1:2])
ESF_unidades_ESTA_2019$CO_TIPO_ESTABELECIMENTO <- as.numeric(
  ifelse(
    is.na(ESF_unidades_ESTA_2019$CO_TIPO_ESTABELECIMENTO), 
    paste0("0"), ESF_unidades_ESTA_2019$CO_TIPO_ESTABELECIMENTO))

ESF_unidades_ESTA_Tipo_2019 <- left_join(ESF_unidades_ESTA_2019, tbTIPOEstab, 
                                         by = "CO_TIPO_ESTABELECIMENTO")

#Save.RDS####  
saveRDS(ESF_unidades_ESTA_Tipo_2019, paste0(here("Previne","data","rds"), "/CNES_ESF_unidades_ESTA_Tipo_2019.RDS"))

#3.2 Horarios####
tbEstaHorario2019_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "tbEstabHorarioAtend2019..*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))

tbEstaHorario2019_raw$CO_UNIDADE <- as.numeric(tbEstaHorario2019_raw$CO_UNIDADE)

ESF_unidades_ESTA_Tipo_2019 <- readRDS(paste0(here("Previne","data","rds"), "/CNES_ESF_unidades_ESTA_Tipo_2019.RDS"))

ESF_unidades_ESTA_Tipo_2019$CO_UNIDADE <- as.numeric(ESF_unidades_ESTA_Tipo_2019$CO_UNIDADE)

tbEstaHorario2019 <- subset(tbEstaHorario2019_raw, CO_UNIDADE %in% ESF_unidades_ESTA_Tipo_2019$CO_UNIDADE)
rm(tbEstaHorario2019_raw)
gc()

#Fix data anomalliess
#HR INICIO DO ATENDIMENTO

unique(tbEstaHorario2019$HR_INICIO_ATENDIMENTO)
tbEstaHorario2019$HR_INICIO_ATENDIMENTO <- gsub(" ", "", 
                                                tbEstaHorario2019$HR_INICIO_ATENDIMENTO)
#Convert times to seconds
tbEstaHorario2019 <- separate(tbEstaHorario2019, HR_INICIO_ATENDIMENTO, 
                              into = c("INICIO_hours", "INICIO_min"), sep = ":")
tbEstaHorario2019$INICIO_hours <- as.numeric(tbEstaHorario2019$INICIO_hours)*60*60
tbEstaHorario2019$INICIO_min <- as.numeric(tbEstaHorario2019$INICIO_min)*60
tbEstaHorario2019$INICIO_final <- tbEstaHorario2019$INICIO_hours + tbEstaHorario2019$INICIO_min

#HR FIM DO ATENDIMENTO - Convert times to seconds
tbEstaHorario2019 <- separate(tbEstaHorario2019, HR_FIM_ATENDIMENTO, 
                              into = c("FIM_hours", "FIM_min"), sep = ":")
tbEstaHorario2019$FIM_hours <- as.numeric(tbEstaHorario2019$FIM_hours)*60*60
tbEstaHorario2019$FIM_min <- as.numeric(tbEstaHorario2019$FIM_min)*60
tbEstaHorario2019$FIM_final <- tbEstaHorario2019$FIM_hours + tbEstaHorario2019$FIM_min

tbEstaHorario2019$Total_HRs_Open <- tbEstaHorario2019$FIM_final - tbEstaHorario2019$INICIO_final

#Summarise hours worked by week
tic()
tbEstaHorario2019_sum <- tbEstaHorario2019 %>%
  group_by(CO_UNIDADE, filename) %>%
  summarise(Total_Hrs_open = sum(Total_HRs_Open)/60/60)
toc()

#Unidades working more tha 60 and 75hrs/week
tbEstaHorario2019_sum$Hrs_60 <- ifelse(tbEstaHorario2019_sum$Total_Hrs_open >= 60, 1, 0)
tbEstaHorario2019_sum$Hrs_75 <- ifelse(tbEstaHorario2019_sum$Total_Hrs_open >= 75, 1, 0)

#Merge  UNI and ESF to get the names of DS tipos

#Save.RDS####
saveRDS(tbEstaHorario2019_sum, paste0(here("Previne","data","rds"), "/CNES_ESF_Horario2019.RDS"))

CNES_ESF_HorarioExt <- readRDS(paste0(here("Previne","data","rds"), "/CNES_ESF_Horario2019.RDS"))

CNES_ESF_HorarioExt$CO_UNIDADE <- as.numeric(CNES_ESF_HorarioExt$CO_UNIDADE)

CNES_ESF_HorarioExt_2 <- left_join(CNES_ESF_HorarioExt, ESF_unidades_ESTA_Tipo_2019, by = c("CO_UNIDADE"))

CNES_ESF_HorarioExt_2$Hrs_ate_59hrs <- ifelse(CNES_ESF_HorarioExt_2$Total_Hrs_open < 60, 1,0)


#Adjust Month and Year
CNES_ESF_HorarioExt_2 <- separate(CNES_ESF_HorarioExt_2, filename, 
                                  into = c("filepath", "Ano.Mes"), sep = "tbEstabHorarioAtend")

CNES_ESF_HorarioExt_2$Ano.Mes <- gsub(".csv", "/01", CNES_ESF_HorarioExt_2$Ano.Mes)
CNES_ESF_HorarioExt_2$Ano.Mes <- gsub("2019", "2019/", CNES_ESF_HorarioExt_2$Ano.Mes)

CNES_ESF_HorarioExt_2$Ano.Mes <- ymd(CNES_ESF_HorarioExt_2$Ano.Mes)
CNES_ESF_HorarioExt_2$Mes <- month(CNES_ESF_HorarioExt_2$Ano.Mes)
CNES_ESF_HorarioExt_2$Ano <- year(CNES_ESF_HorarioExt_2$Ano.Mes)

#Group to reduce
CNES_ESF_HorarioExt_3 <- CNES_ESF_HorarioExt_2 %>%
  group_by(CO_MUNICIPIO_GESTOR, Mes, Ano, TP_GESTAO, 
           CO_ATIVIDADE_PRINCIPAL, CO_TIPO_ESTABELECIMENTO, DS_TIPO_ESTABELECIMENTO) %>%
  summarise(Hrs_ate_59hrs = sum(Hrs_ate_59hrs), Hrs_60 = sum(Hrs_60), Hrs_75 = sum(Hrs_75))

#Save.RDS####
saveRDS(CNES_ESF_HorarioExt_2, paste0(here("Previne","data","rds"), "/CNES_ESF_Horario2019_full.RDS"))
saveRDS(CNES_ESF_HorarioExt_3, paste0(here("Previne","data","rds"), "/CNES_ESF_Horario2019.RDS"))


#4. VAR:    numero de profissionais de saude (ASC, medicos, enfermeiras etc) ####
#SOURSE:  + RL_ESTAB_EQUIPE_PROF2019XX.csv

tbProfissionais2019_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "rlEstabEquipeProf2019..*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))

tbProfissionais2019 <- tbProfissionais2019_raw %>%
  filter(TP_SUS_NAO_SUS == "S") %>%
  filter(DT_DESLIGAMENTO == "") %>%
  group_by(CO_MUNICIPIO, CO_UNIDADE, CO_CBO, CO_MUN_ATUACAO, filename) %>%
  summarise(count_CO_CBO = n())

rm(tbProfissionais2019_raw)
gc()

tbProfissionais2019$CO_CBO_group <- stringr::str_extract(
  tbProfissionais2019$CO_CBO, "^.{3}")
tbProfissionais2019$CO_CBO_subgroup <- stringr::str_extract(
  tbProfissionais2019$CO_CBO, "^.{4}")

# CBO_Prof_Enfermeiro <- 2235 
# CBO_Profs_da_Medico <- 225 
# CBO_Prof_TecAux_Medico <- 3222
# CBO_Prof_Dentista <- 2232
# CBO_Prof_TecAux_Dentista <- 3224

perfis_medico <- c("225")
perfis_enfermeirx_e_tec <- c("2235", "3222")
perfis_dentista_e_aux <- c("2232", "3224")

perfis_enf_dent_tec_aux <- c("2235", "3222","2232", "3224")

CBO2002_PerfilOcupacional <- read.csv("~/Previne/data/raw/ESTRUTURA CBO/CBO2002 - Ocupacao.csv", sep = ";", stringsAsFactors = FALSE)


CBO2002_PerfilOcupacional$CO_CBO_group <-  stringr::str_extract(
  CBO2002_PerfilOcupacional$CODIGO, "^.{3}")

CBO2002_PerfilOcupacional$CO_CBO_subgroup <-  stringr::str_extract(
  CBO2002_PerfilOcupacional$CODIGO, "^.{4}")


CBO2002_short <- CBO2002_PerfilOcupacional %>%
  filter(CO_CBO_subgroup %in%  perfis_enf_dent_tec_aux | CO_CBO_group == 225)
#Add names to tbProfissionais 

colnames(CBO2002_short)[1] <- "CO_CBO"

CBO2002_short$CO_CBO <- as.character(CBO2002_short$CO_CBO)

tbProfissionais2019_names <- left_join(tbProfissionais2019, CBO2002_short,
                                       by = "CO_CBO" )

rm(tbProfissionais2019)

tbProfissionais2019_names <- tbProfissionais2019_names[,-c(10:11)]

tbProfissionais2019_names$CO_UNIDADE <- as.character(tbProfissionais2019_names$CO_UNIDADE)


#Load file with unique Unidades per Municipality

rlEstabProgFundo_Gestao_unique <- readRDS(paste0(here("Previne","data","rds"), "/CNES_EstabProgFundo_APS_Unique_2019.RDS"))

CNES_Profissionais2019_APS <- tbProfissionais2019_names[
  tbProfissionais2019_names$CO_UNIDADE %in% rlEstabProgFundo_Gestao_unique$CO_UNIDADE,]

#Calculate number of professionals per municipality
CNES_Profissionais2019_APS$Medico <- ifelse(
  CNES_Profissionais2019_APS$CO_CBO_group.x == perfis_medico, 1, 0)

CNES_Profissionais2019_APS$Enfermeirx <- ifelse(
  CNES_Profissionais2019_APS$CO_CBO_subgroup.x %in% perfis_enfermeirx_e_tec, 1, 0)

CNES_Profissionais2019_APS$Prof_Bucal <- ifelse(
  CNES_Profissionais2019_APS$CO_CBO_subgroup.x %in% perfis_dentista_e_aux, 1, 0)

#Group_by and Summarise by Munic

##Adjust year and month

CNES_Profissionais2019_APS <- separate(CNES_Profissionais2019_APS, filename, 
                                       into = c("filepath", "Ano.Mes"), sep = "rlEstabEquipeProf")

CNES_Profissionais2019_APS$Ano.Mes <- gsub(".csv", "/01", CNES_Profissionais2019_APS$Ano.Mes)
CNES_Profissionais2019_APS$Ano.Mes <- gsub("2019", "2019/", CNES_Profissionais2019_APS$Ano.Mes)

CNES_Profissionais2019_APS$Ano.Mes <- ymd(CNES_Profissionais2019_APS$Ano.Mes)
CNES_Profissionais2019_APS$Mes <- month(CNES_Profissionais2019_APS$Ano.Mes)
CNES_Profissionais2019_APS$Ano <- year(CNES_Profissionais2019_APS$Ano.Mes)

CNES_Profissionais2019_APS_sum <- CNES_Profissionais2019_APS %>%
  group_by(CO_MUNICIPIO, Mes, Ano) %>%
  summarise(Medico = sum(Medico), Enfermeirx = sum(Enfermeirx), Prof_Bucal = sum(Prof_Bucal))

#To simplify the analysis converge COD_MUN and COD_Atuação 
# CNES_Profissionais2019_APS_sum$dif_CO_mu_atu <- 
#   CNES_Profissionais2019_APS_sum$CO_MUNICIPIO == CNES_Profissionais2019_APS_sum$CO_MUN_ATUACAO
# 
# CNES_Profissionais2019_APS_sum$CO_MUNICIPIO <- ifelse(
#   CNES_Profissionais2019_APS_sum$dif_CO_mu_atu == TRUE, 
#   CNES_Profissionais2019_APS_sum$CO_MUNICIPIO, paste0(CNES_Profissionais2019_APS_sum$CO_MUN_ATUACAO))

#Save.RDS####    
saveRDS(CNES_Profissionais2019_APS, paste0(here("Previne","data","rds"), "/CNES_Profissionais_APS_full_2019.RDS"))
saveRDS(CNES_Profissionais2019_APS_sum, paste0(here("Previne","data","rds"), "/CNES_Profissionais_APS_2019.RDS"))

