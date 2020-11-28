#CNES Variables

#Section 1
  #VAR:    O n?mero de Equipes da Aten??o B?sica (EAB)
  #VAR:    O n?mero de Equipes da ESF
    #SOURCE: "tbEquipe202008"
      #Output file =  ESF_equipes_2020.RDS

#Section 2
  #VAR:    Popula??o do Municipio
    #SOURCE: tbMunicipio202008.csv
      #Output file =  Municipios.RDS

#Section 3
  #VAR:    Percentual de UBS que adotaram horario de atendimento estendido
    #SOURCE: tbEstabelicimentos2020XX.csv + tbEstabelicimentosHorario2020XX.csv
      #Output file =  ESF_Horario2020.RDS
      #Output file =  ESF_Horario2020_full.RDS
      #Output file =  CNES_EstabProgFundo_APS_Unique_2020.RDS 
      #Output file =  CNES_Estabelecimentos_APS_2020.RDS

#Section 4
  #VAR:    numero de profissionais de saude (ASC, medicos, enfermeiras etc) 
    #SOURSE:  + RL_ESTAB_EQUIPE_PROF2020XX.csv
    #SOURCE: https://integracao.esusab.ufsc.br/v211/docs/cbo.html
      #Output file =    CNES_Profissionais_APS_full.RDS
      #Output file =    CNES_Profissionais_APS.RDS

#VAR:    numero de leitos e hospitais por n. de leitos (ver com Stella Lobo)
#SOURCE: { CNES} RL_ESTAB_COMPLEMENTAR2020XX.csv + leitos

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
  #SOURCE: tbEstabelecimento2 tbEquipe2020..

tbEquipe2020_raw <- 
  list.files(path = here("Previne","data", "raw", "CNES_bases"), 
             pattern = "tbEquipe2020..*?",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))


##Adjust year and month
tbEquipe2020_raw <- separate(tbEquipe2020_raw, filename, into = c("filepath", "Ano.Mes"), sep = "tbEquipe")

tbEquipe2020_raw$Ano.Mes <- gsub(".csv", "/01", tbEquipe2020_raw$Ano.Mes)
tbEquipe2020_raw$Ano.Mes <- gsub("2020", "2020/", tbEquipe2020_raw$Ano.Mes)

##Summarise
Equipes_short <- tbEquipe2020_raw %>% 
  group_by(CO_MUNICIPIO,CO_UNIDADE, TP_EQUIPE, Ano.Mes) %>% 
  summarise(count = n())

portaria99_ajuste_tipologia_equipes <- read.csv("~/Previne/data/portaria99_ajuste_tipologia_equipes.csv")

colnames(portaria99_ajuste_tipologia_equipes)[1] <- "TP_EQUIPE"
colnames(portaria99_ajuste_tipologia_equipes)[4] <- "NEW_TP_EQUIPE"

Equipes_2020 <- left_join(Equipes_short, portaria99_ajuste_tipologia_equipes, by = c("TP_EQUIPE") )

ESF_Equipes_2020 <- Equipes_2020 %>%
  filter(NEW_TP_EQUIPE == 70 | NEW_TP_EQUIPE == 76) %>% #ESF teams only
  group_by(CO_MUNICIPIO, CO_UNIDADE, NEW_TP_EQUIPE,Portaria_n99_7Fev2020, Ano.Mes) %>%
  summarise(count = n())

ESF_Unidades <-   as.data.frame(c(ESF_Equipes_2020$CO_UNIDADE, ESF_Equipes_2020$CO_MUNICIPIO))

CNES_ESF_Equipes_short <- ESF_Equipes_2020 %>%
  group_by(CO_MUNICIPIO, Portaria_n99_7Fev2020, Ano.Mes) %>%
  summarise(Qtd = sum(count))

CNES_ESF_Equipes_short$Ano.Mes <- ymd(CNES_ESF_Equipes_short$Ano.Mes)
CNES_ESF_Equipes_short$Mes <- month(CNES_ESF_Equipes_short$Ano.Mes)
CNES_ESF_Equipes_short$Ano <- year(CNES_ESF_Equipes_short$Ano.Mes)

    #Save.RDS####
saveRDS(ESF_Equipes_2020, paste0(here("Previne","data","rds"), "/CNES_ESF_equipes_2020_full.RDS"))
saveRDS(CNES_ESF_Equipes_short, paste0(here("Previne","data","rds"), "/CNES_ESF_equipes_2020.RDS"))

saveRDS(ESF_Unidades, paste0(here("Previne","data","rds"), "/CNES_ESF_unidades_2020.RDS"))

#2. VAR:    Popula?ao Municipio####  

tbMun2020_raw <- 
    list.files(path = here("Previne","data", "raw", "CNES_bases"), 
               pattern = "tbMunicipio202001.csv?",
               recursive = T,
               full.names = T) %>%
    map_df(~read_plus(.))
    #Save.RDS####  
saveRDS(tbMun2020_raw, paste0(here("Previne","data","rds"), "/Municipios.RDS"))


#3. VAR:    % de UBS com horario de atendimento estendido####
#SOURCE: tbEstabelicimentos2020XX.csv + tbEstabelicimentosHorario2020XX.csv
  
  #3.0 Nivel de aten?ao dos estabelecimentos####
  rlEstabProgFundo202008 <- 
    list.files(path = here("Previne","data", "raw", "CNES_bases"), 
               pattern = "rlEstabProgFundo2020..*?",
               recursive = T,
               full.names = T) %>%
    map_df(~read_plus(.))
  
    #Tipo de aten?ao
    tbGestao2020_raw <- 
      list.files(path = here("Previne","data", "raw", "CNES_bases"), 
                 pattern = "tbGestao2020..*?",
                 recursive = T,
                 full.names = T) %>%
      map_df(~read_plus(.))
    tbGestao2020 <- tbGestao2020_raw %>%
      group_by(CO_GESTAO,DS_GESTAO,TP_PROG) %>%
      summarise(n = n())
    tbGestao2020 <- tbGestao2020[, -4]
    rm(tbGestao2020_raw)
    
    colnames(tbGestao2020)[1] <- "CO_ATIVIDADE"
    
    rlEstabProgFundo_Gestao <- left_join(rlEstabProgFundo202008, tbGestao2020, 
                                         by = "CO_ATIVIDADE" )
    
    rm(rlEstabProgFundo202008)
    
    rlEstabProgFundo_Gestao_unique <- rlEstabProgFundo_Gestao %>%
      filter(CO_ATIVIDADE == 1) %>%
      group_by(CO_UNIDADE, CO_ATIVIDADE, DS_GESTAO) %>%
      summarise(n = n())
    
    rm(rlEstabProgFundo_Gestao)
    rm(tbGestao2020)
    gc()
    
  #3.1 Estabelecimentos####
    #Special function to call and filter estabelecimentos da APS
    read_plus_filter <- function(flnm) {
      fread(flnm) %>% 
        mutate(filename = flnm) %>%
        filter(CO_UNIDADE %in% rlEstabProgFundo_Gestao_unique$CO_UNIDADE)
    }
    
    
    tbEstabelecimentos_raw <- 
    list.files(path = here("Previne","data", "raw", "CNES_bases"), 
               pattern = "tbEstabelecimento2020..*?",
               recursive = T,
               full.names = T) %>%
    map_df(~read_plus_filter(.))
  
    
  tbEstabelecimentos <- tbEstabelecimentos_raw[, c(1,2,12,22,23,29, 30, 32, 40, 41, 50, 52, 53)]
  
  rm(tbEstabelecimentos_raw)
  gc()
  
  
    #Save.RDS####  
    
  saveRDS(rlEstabProgFundo_Gestao_unique, paste0(here("Previne","data","rds"),
                                                 "/CNES_EstabProgFundo_APS_Unique_2020.RDS"))
  
  saveRDS(tbEstabelecimentos, paste0(here("Previne","data","rds"), "/CNES_Estabelecimentos_APS_2020.RDS"))
  
  
    #Filtre por unidades relevantes
      ESF_unidades_2020 <- readRDS(paste0(here("Previne","data","rds"), "/CNES_ESF_equipes_2020_full.RDS"))
      ESF_unidades_2020_unique <- as.data.frame(unique(ESF_unidades_2020$CO_UNIDADE))
      rm(ESF_unidades_2020)
      ESF_unidades_2020_unique$x <- as.character(ESF_unidades_2020_unique$x) 
      
      #Filtre por ESFs apenas
      ESF_unidades_ESTA_2020 <- subset(tbEstabelecimentos, CO_UNIDADE %in% ESF_unidades_2020_unique$x)
  
    #Tipos de Estabelecimentos
      tbTIPOEstab_raw <- 
        list.files(path = here("Previne","data", "raw", "CNES_bases"), 
                   pattern = "tbTipoEstabelecimento202008*?",
                   recursive = T,
                   full.names = T) %>%
        map_df(~read_plus(.))
      
        tbTIPOEstab <- unique(tbTIPOEstab_raw[,1:2])
        ESF_unidades_ESTA_2020$CO_TIPO_ESTABELECIMENTO <- as.numeric(
          ifelse(
            is.na(ESF_unidades_ESTA_2020$CO_TIPO_ESTABELECIMENTO), 
            paste0("0"), ESF_unidades_ESTA_2020$CO_TIPO_ESTABELECIMENTO))
    
    ESF_unidades_ESTA_Tipo_2020 <- left_join(ESF_unidades_ESTA_2020, tbTIPOEstab, 
                                           by = "CO_TIPO_ESTABELECIMENTO")
    
  #Save.RDS####  
  saveRDS(ESF_unidades_ESTA_Tipo_2020, paste0(here("Previne","data","rds"), "/CNES_ESF_unidades_ESTA_Tipo_2020.RDS"))
  
  #3.2 Horarios####
  tbEstaHorario2020_raw <- 
    list.files(path = here("Previne","data", "raw", "CNES_bases"), 
               pattern = "tbEstabHorarioAtend2020..*?",
               recursive = T,
               full.names = T) %>%
    map_df(~read_plus(.))

  tbEstaHorario2020_raw$CO_UNIDADE <- as.numeric(tbEstaHorario2020_raw$CO_UNIDADE)
  
  ESF_unidades_ESTA_Tipo_2020 <- readRDS(paste0(here("Previne","data","rds"), "/CNES_ESF_unidades_ESTA_Tipo_2020.RDS"))
  
  ESF_unidades_ESTA_Tipo_2020$CO_UNIDADE <- as.numeric(ESF_unidades_ESTA_Tipo_2020$CO_UNIDADE)
  
  tbEstaHorario2020 <- subset(tbEstaHorario2020_raw, CO_UNIDADE %in% ESF_unidades_ESTA_Tipo_2020$CO_UNIDADE)
  rm(tbEstaHorario2020_raw)
  gc()
  
  #Fix data anomalliess
  #HR INICIO DO ATENDIMENTO
  
    unique(tbEstaHorario2020$HR_INICIO_ATENDIMENTO)
    tbEstaHorario2020$HR_INICIO_ATENDIMENTO <- gsub(" ", "", 
                                                    tbEstaHorario2020$HR_INICIO_ATENDIMENTO)
    #Convert times to seconds
    tbEstaHorario2020 <- separate(tbEstaHorario2020, HR_INICIO_ATENDIMENTO, 
                                  into = c("INICIO_hours", "INICIO_min"), sep = ":")
    tbEstaHorario2020$INICIO_hours <- as.numeric(tbEstaHorario2020$INICIO_hours)*60*60
    tbEstaHorario2020$INICIO_min <- as.numeric(tbEstaHorario2020$INICIO_min)*60
    tbEstaHorario2020$INICIO_final <- tbEstaHorario2020$INICIO_hours + tbEstaHorario2020$INICIO_min
    
    #HR FIM DO ATENDIMENTO - Convert times to seconds
    tbEstaHorario2020 <- separate(tbEstaHorario2020, HR_FIM_ATENDIMENTO, 
                                  into = c("FIM_hours", "FIM_min"), sep = ":")
    tbEstaHorario2020$FIM_hours <- as.numeric(tbEstaHorario2020$FIM_hours)*60*60
    tbEstaHorario2020$FIM_min <- as.numeric(tbEstaHorario2020$FIM_min)*60
    tbEstaHorario2020$FIM_final <- tbEstaHorario2020$FIM_hours + tbEstaHorario2020$FIM_min
    
    tbEstaHorario2020$Total_HRs_Open <- tbEstaHorario2020$FIM_final - tbEstaHorario2020$INICIO_final
  
    #Summarise hours worked by week
    tic()
    tbEstaHorario2020_sum <- tbEstaHorario2020 %>%
      group_by(CO_UNIDADE, filename) %>%
      summarise(Total_Hrs_open = sum(Total_HRs_Open)/60/60)
    toc()

    #Unidades working more tha 60 and 75hrs/week
    tbEstaHorario2020_sum$Hrs_60 <- ifelse(tbEstaHorario2020_sum$Total_Hrs_open >= 60, 1, 0)
    tbEstaHorario2020_sum$Hrs_75 <- ifelse(tbEstaHorario2020_sum$Total_Hrs_open >= 75, 1, 0)
  
    #Merge  UNI and ESF to get the names of DS tipos
    CNES_ESF_HorarioExt <- readRDS(paste0(here("Previne","data","rds"), "/CNES_ESF_Horario2020.RDS"))
    
    CNES_ESF_HorarioExt$CO_UNIDADE <- as.character(CNES_ESF_HorarioExt$CO_UNIDADE)
    
    CNES_ESF_HorarioExt_2 <- left_join(CNES_ESF_HorarioExt, CNES_Uni_Esta_Tipo, by = c("CO_UNIDADE"))
    
    CNES_ESF_HorarioExt_2$Hrs_ate_59hrs <- ifelse(CNES_ESF_HorarioExt_2$Total_Hrs_open < 60, 1,0)
    
   
    #Adjust Month and Year
    CNES_ESF_HorarioExt_2 <- separate(CNES_ESF_HorarioExt_2, filename, 
                                           into = c("filepath", "Ano.Mes"), sep = "tbEstabHorarioAtend")
    
    CNES_ESF_HorarioExt_2$Ano.Mes <- gsub(".csv", "/01", CNES_ESF_HorarioExt_2$Ano.Mes)
    CNES_ESF_HorarioExt_2$Ano.Mes <- gsub("2020", "2020/", CNES_ESF_HorarioExt_2$Ano.Mes)
    
    CNES_ESF_HorarioExt_2$Ano.Mes <- ymd(CNES_ESF_HorarioExt_2$Ano.Mes)
    CNES_ESF_HorarioExt_2$Mes <- month(CNES_ESF_HorarioExt_2$Ano.Mes)
    CNES_ESF_HorarioExt_2$Ano <- year(CNES_ESF_HorarioExt_2$Ano.Mes)
   
    #Group to reduce
    CNES_ESF_HorarioExt_3 <- CNES_ESF_HorarioExt_2 %>%
      group_by(CO_MUNICIPIO_GESTOR, Mes, Ano, TP_GESTAO, 
               CO_ATIVIDADE_PRINCIPAL, CO_TIPO_ESTABELECIMENTO, DS_TIPO_ESTABELECIMENTO) %>%
      summarise(Hrs_ate_59hrs = sum(Hrs_ate_59hrs), Hrs_60 = sum(Hrs_60), Hrs_75 = sum(Hrs_75))
     
    #Save.RDS####
      saveRDS(CNES_ESF_HorarioExt_2, paste0(here("Previne","data","rds"), "/CNES_ESF_Horario2020_full.RDS"))
      saveRDS(CNES_ESF_HorarioExt_3, paste0(here("Previne","data","rds"), "/CNES_ESF_Horario2020.RDS"))
    
     
#4. VAR:    numero de profissionais de saude (ASC, medicos, enfermeiras etc) ####
  #SOURSE:  + RL_ESTAB_EQUIPE_PROF2020XX.csv
  
  tbProfissionais2020_raw <- 
    list.files(path = here("Previne","data", "raw", "CNES_bases"), 
               pattern = "rlEstabEquipeProf2020..*?",
               recursive = T,
               full.names = T) %>%
    map_df(~read_plus(.))
  
    tbProfissionais2020 <- tbProfissionais2020_raw %>%
      filter(TP_SUS_NAO_SUS == "S") %>%
      filter(DT_DESLIGAMENTO == "") %>%
      group_by(CO_MUNICIPIO, CO_UNIDADE, CO_CBO, CO_MUN_ATUACAO, filename) %>%
      summarise(count_CO_CBO = n())
  
    rm(tbProfissionais2020_raw)
    gc()
    
    tbProfissionais2020$CO_CBO_group <- stringr::str_extract(
      tbProfissionais2020$CO_CBO, "^.{3}")
    tbProfissionais2020$CO_CBO_subgroup <- stringr::str_extract(
      tbProfissionais2020$CO_CBO, "^.{4}")
  
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
    
    tbProfissionais2020_names <- left_join(tbProfissionais2020, CBO2002_short,
                                           by = "CO_CBO" )
    
    rm(tbProfissionais2020)
    
    tbProfissionais2020_names <- tbProfissionais2020_names[,-c(10:11)]
    
    tbProfissionais2020_names$CO_UNIDADE <- as.character(tbProfissionais2020_names$CO_UNIDADE)
   
    
    #Load file with unique Unidades per Municipality
    
    rlEstabProgFundo_Gestao_unique <- readRDS(paste0(here("Previne","data","rds"), "/CNES_EstabProgFundo_APS_Unique_2020.RDS"))
    
    CNES_Profissionais2020_APS <- tbProfissionais2020_names[
      tbProfissionais2020_names$CO_UNIDADE %in% rlEstabProgFundo_Gestao_unique$CO_UNIDADE,]
    
    #Calculate number of professionals per municipality
       CNES_Profissionais2020_APS$Medico <- ifelse(
         CNES_Profissionais2020_APS$CO_CBO_group.x == perfis_medico, 1, 0)
       
       CNES_Profissionais2020_APS$Enfermeirx <- ifelse(
         CNES_Profissionais2020_APS$CO_CBO_subgroup.x %in% perfis_enfermeirx_e_tec, 1, 0)
      
       CNES_Profissionais2020_APS$Prof_Bucal <- ifelse(
        CNES_Profissionais2020_APS$CO_CBO_subgroup.x %in% perfis_dentista_e_aux, 1, 0)
     
      #Group_by and Summarise by Munic
      
       ##Adjust year and month
      
       CNES_Profissionais2020_APS <- separate(CNES_Profissionais2020_APS, filename, 
                                              into = c("filepath", "Ano.Mes"), sep = "rlEstabEquipeProf")
       
       CNES_Profissionais2020_APS$Ano.Mes <- gsub(".csv", "/01", CNES_Profissionais2020_APS$Ano.Mes)
       CNES_Profissionais2020_APS$Ano.Mes <- gsub("2020", "2020/", CNES_Profissionais2020_APS$Ano.Mes)
       
       CNES_Profissionais2020_APS$Ano.Mes <- ymd(CNES_Profissionais2020_APS$Ano.Mes)
       CNES_Profissionais2020_APS$Mes <- month(CNES_Profissionais2020_APS$Ano.Mes)
       CNES_Profissionais2020_APS$Ano <- year(CNES_Profissionais2020_APS$Ano.Mes)
       
       CNES_Profissionais2020_APS_sum <- CNES_Profissionais2020_APS %>%
         group_by(CO_MUNICIPIO, Mes, Ano) %>%
         summarise(Medico = sum(Medico), Enfermeirx = sum(Enfermeirx), Prof_Bucal = sum(Prof_Bucal))
       
       #To simplify the analysis converge COD_MUN and COD_Atua??o 
       # CNES_Profissionais2020_APS_sum$dif_CO_mu_atu <- 
       #   CNES_Profissionais2020_APS_sum$CO_MUNICIPIO == CNES_Profissionais2020_APS_sum$CO_MUN_ATUACAO
       # 
       # CNES_Profissionais2020_APS_sum$CO_MUNICIPIO <- ifelse(
       #   CNES_Profissionais2020_APS_sum$dif_CO_mu_atu == TRUE, 
       #   CNES_Profissionais2020_APS_sum$CO_MUNICIPIO, paste0(CNES_Profissionais2020_APS_sum$CO_MUN_ATUACAO))
       
    #Save.RDS####    
    saveRDS(CNES_Profissionais2020_APS, paste0(here("Previne","data","rds"), "/CNES_Profissionais_APS_full.RDS"))
    saveRDS(CNES_Profissionais2020_APS_sum, paste0(here("Previne","data","rds"), "/CNES_Profissionais_APS.RDS"))
 
       