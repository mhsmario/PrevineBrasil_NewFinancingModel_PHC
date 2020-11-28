# COVID-19 data
  #VAR = Numero de Casos por mes + Numero de obitos por mes
    #source = https://covid.saude.gov.br/
      #Output file = COVID_Aug31.RDS"


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
library(xlsx)

setwd(here("Previne"))
#Load data####
    HIST_PAINEL_COVIDBR_28set2020 <- read_csv("data/raw/Covid/HIST_PAINEL_COVIDBR_28set2020.csv",
                                              col_types = cols(
                                                codmun = col_character(), coduf = col_character(),
                                                estado = col_character(), municipio = col_character(), 
                                                nomeRegiaoSaude = col_character()),
                                              na = "NA")
    
    COVID19_28Set_2020 <- HIST_PAINEL_COVIDBR_28set2020

#Clean data####
    COVID19_28Set_2020$codmun <- ifelse(COVID19_28Set_2020$coduf == 76, 
                                                   paste0("760000"), paste0(COVID19_28Set_2020$codmun))
    
    #Convert data to date class using lubridate
    COVID19_28Set_2020$data <- mdy(COVID19_28Set_2020$data)
    COVID19_28Set_2020$mes <- month(COVID19_28Set_2020$data)
    COVID19_28Set_2020$dia <- day(COVID19_28Set_2020$data)
    
    #Note that amounts per day are already aggregates! 
    #Keep only the last day of the month to have the months total.
    COVID19_28.09_short <- COVID19_28Set_2020 %>%
      group_by(regiao, coduf, codmun, municipio, mes, populacaoTCU2019) %>%
      filter(codmun != "" | 76) %>%
      filter(dia == 30 | dia == 31)
    
      COVID19_28.09_short$delete <- ifelse(COVID19_28.09_short$mes == 3 & COVID19_28.09_short$dia == 30, 
                                           1, 0)
      
      COVID19_28.09_short$delete <- ifelse(COVID19_28.09_short$mes == 5 & COVID19_28.09_short$dia == 30, 1, 
                                           COVID19_28.09_short$delete)
      COVID19_28.09_short$delete <- ifelse(COVID19_28.09_short$mes == 7 & COVID19_28.09_short$dia == 30, 1, 
                                           COVID19_28.09_short$delete)
      COVID19_28.09_short$delete <- ifelse(COVID19_28.09_short$mes == 8 & COVID19_28.09_short$dia == 30, 1, 
                                           COVID19_28.09_short$delete)
    
      COVID19_31.08 <- COVID19_28.09_short %>%
        filter(delete == 0)

#Calculate cases in each month (disaggregated) for CASOS####

  COVID19_31.08$data <- as.character(COVID19_31.08$data)

  COVID19_31.08_casos <- COVID19_31.08[, -c(9, 12:20)]
  
  #Convert df from long to wide format
    COVID_wide_casos <- COVID19_31.08_casos %>%
      group_by(regiao, estado, municipio, coduf, codmun, populacaoTCU2019) %>%
      pivot_wider(names_from = data, values_from = casosAcumulado)
    
    COVID_wide_casos$cases_mar <-  COVID_wide_casos$`2020-03-31`
    COVID_wide_casos$cases_apr <- COVID_wide_casos$`2020-04-30` - COVID_wide_casos$`2020-03-31`
    COVID_wide_casos$cases_may <- COVID_wide_casos$`2020-05-31` - COVID_wide_casos$`2020-04-30`
    COVID_wide_casos$cases_june <- COVID_wide_casos$`2020-06-30` - COVID_wide_casos$`2020-05-31`
    COVID_wide_casos$cases_june <- COVID_wide_casos$`2020-07-31` - COVID_wide_casos$`2020-06-30`
    COVID_wide_casos$cases_july <- COVID_wide_casos$`2020-08-31` - COVID_wide_casos$`2020-07-31`
    COVID_wide_casos$cases_ago <-  0.1
  
    #Convert WIDE_CASOS back to long
    
    COVID_long_casos <- COVID_wide_casos %>%
      pivot_longer(cols = starts_with("cases_"),
                   names_to = "mes-previous",
                   values_to = "casos_diff_mes")  
  
#Calculate cases in each month (disaggregated) for OBITOS####
  
  COVID19_31.08$data <- as.character(COVID19_31.08$data)
  
  COVID19_31.08_obitos <- COVID19_31.08[, -c(9, 11,12,14:20)]
  
  #Convert df from long to wide format
  COVID_wide_obt <- COVID19_31.08_obitos %>%
    group_by(regiao, estado, municipio, coduf, codmun, populacaoTCU2019) %>%
    pivot_wider(names_from = data, values_from = obitosAcumulado)
  
  COVID_wide_obt$obitos_mar <-  COVID_wide_obt$`2020-03-31`
  COVID_wide_obt$obitos_apr <- COVID_wide_obt$`2020-04-30` - COVID_wide_obt$`2020-03-31`
  COVID_wide_obt$obitos_may <- COVID_wide_obt$`2020-05-31` - COVID_wide_obt$`2020-04-30`
  COVID_wide_obt$obitos_june <- COVID_wide_obt$`2020-06-30` - COVID_wide_obt$`2020-05-31`
  COVID_wide_obt$obitos_june <- COVID_wide_obt$`2020-07-31` - COVID_wide_obt$`2020-06-30`
  COVID_wide_obt$obitos_july <- COVID_wide_obt$`2020-08-31` - COVID_wide_obt$`2020-07-31`
  COVID_wide_obt$obitos_ago <- 0.1 

  #Convert WIDE_OBITOS back to long

  COVID_long_obt <- COVID_wide_obt %>%
    pivot_longer(cols = starts_with("obitos_"),
                 names_to = "mes-previous_obt",
                 values_to = "casos_diff_mes_obt")
  
#Add calculated variables to final dataset####  
  COVID19_31.08$casos_diff_mes <- COVID_long_casos$casos_diff_mes
  COVID19_31.08$obitos_dif_mes <- COVID_long_obt$casos_diff_mes_obt
  COVID19_31.08 <- COVID19_31.08[,-20]
  
  COVID19_31.08 <- COVID19_31.08[,c(1:7,9,8,18,19,10,11,20,12,13,21,14,15,16,17)]
#Filter rows without Municipality number
  COVID19_31.08_ <- COVID19_31.08 %>%
    filter(codmun != "")
  
  
#Save.RDS####  
 saveRDS(COVID19_31.08, paste0(here("Previne","data","rds"), "/COVID_Aug31.RDS"))
  