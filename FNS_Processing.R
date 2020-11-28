#FNS and SISAB data


library(dplyr)
library(here)
library(tidyr)
library(stringi)

#install.packages(c("dplyr", "here", "tidyr", "stringi"))

rm(list = ls())

#setwd(here("Previne"))
##PreLoad####
# FNS_APS_2020 <- readRDS(here("Previne/data/rds/FNS_APS_data_2020.RDS"))
# FNS_APS_2019 <- readRDS(here("Previne/data/rds/FNS_APS_data_2019.RDS"))
# FNS_APS_2018 <- readRDS(here("Previne/data/rds/FNS_APS_data_2018.RDS"))
# FNS_APS_ALL <- rbind(FNS_APS_2020,FNS_APS_2019,FNS_APS_2018)
# 
# ###Save.RDS ALL YEARS####
# saveRDS(FNS_APS_ALL, here("Previne/data/rds/FNS_APS_data_ALL.RDS"))
# rm(FNS_APS_2020,FNS_APS_2019,FNS_APS_2018)

####Load####
FNS_APS_ALL <- readRDS(here("Previne/data/rds/FNS_APS_data_ALL.RDS"))
ano = readline("Please enter the year (2018, 2019, or 2020) to filter and export the data.")
ano <- as.numeric(ano)

ifelse(ano > 2020, break, print("The year selected was: ",ano))
ifelse(ano < 2018, break, print("The year selected was: ",ano))

FNS_APS <- FNS_APS_ALL %>% filter(Compt_ano == ano)
sapply(FNS_APS, function(x) sum(is.na(x)))
###Fixing Month and Year####

##Checking action codes####
FNS_APS$acao <- FNS_APS$Codigo
FNS_APS$acao <- gsub("62059", "PAB.Variavel", FNS_APS$acao)
FNS_APS$acao <- gsub("62060", "Ag.Comun.Saude", FNS_APS$acao)
FNS_APS$acao <- gsub("62061", "Custeio.AS.Bucal", FNS_APS$acao)
FNS_APS$acao <- gsub("62062", "Prg.Informatizacao.UBS", FNS_APS$acao)
FNS_APS$acao <- gsub("62458", "Apoio.Academia.da.Saude", FNS_APS$acao)
FNS_APS$acao <- gsub("64758", "RAPS.CRACK", FNS_APS$acao)
FNS_APS$acao <- gsub("65058", "Incremento.Temp.ABS", FNS_APS$acao)
FNS_APS$acao <- gsub("65578", "Capitacao.Ponderada", FNS_APS$acao)
FNS_APS$acao <- gsub("65580", "Desempenho", FNS_APS$acao)
FNS_APS$acao <- gsub("65582", "Inc.Acoes.Estrategicas", FNS_APS$acao)
FNS_APS$acao <- gsub("65584", "PCapt.de.Transicao", FNS_APS$acao)
FNS_APS$acao <- gsub("65586", "Fator.Compensatorio", FNS_APS$acao)
FNS_APS$acao <- gsub("65588", "Pr.Informatizacao.APS", FNS_APS$acao)

FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "", FNS_APS$nuMes, FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "1", paste0("01"), FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "3", paste0("03"), FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "4", paste0("04"), FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "5", paste0("05"), FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "6", paste0("06"), FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "7", paste0("07"), FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "8", paste0("08"), FNS_APS$Compt_mes)
FNS_APS$Compt_mes <- ifelse(FNS_APS$Compt_mes == "9", paste0("09"), FNS_APS$Compt_mes)

## Summarizing transfers data to get total received by Municipality####
df1 <- FNS_APS %>%
  group_by(sgUf, noMunicipio, Compt_ano,Compt_mes) %>%
  summarise(Valor_Total_Recebido = sum(vlTotal))

df2 <- FNS_APS %>%
  group_by(sgUf, noMunicipio, Compt_ano, Compt_mes, Codigo, acao) %>%
  summarise(Valor_Total_Recebido = sum(vlTotal))

###Calculating weight of transfers####
df3 <- left_join(df2, df1, by = c("sgUf","noMunicipio", "Compt_ano", "Compt_mes"))

df3$pctAcao <- df3$Valor_Total_Recebido.x/df3$Valor_Total_Recebido.y 

df3_greaterthan1 <- df3 %>%
  filter(pctAcao > 1)

###Summarizing and spreading by acao####
df_final_wide <- df3 %>%
  group_by(sgUf, noMunicipio, Compt_ano, Compt_mes, Codigo) %>%
  summarise(pctAcao = sum(pctAcao), Vltotal = mean(Valor_Total_Recebido.y)) %>%
  spread(Codigo, pctAcao)

df_final_long <- df3 %>%
  group_by(sgUf, noMunicipio, Compt_ano, Compt_mes,Codigo, acao) %>%
  summarise(pctAcao = sum(pctAcao), Vltotal = mean(Valor_Total_Recebido.y))

#df_final_wide$Valor_Total_Recebido <- df1$Valor_Total_Recebido

##Fixing column names before joinging dataframes####

rm(df1,df2, df3, df3_greaterthan1)

#Save.RDS####
saveRDS(df_final_long, paste0(here("Previne/data/rds/FNS_data_", ano, "_long.RDS")))
saveRDS(df_final_wide, paste0(here("Previne/data/rds/FNS_data_", ano, "_wide.RDS")))  

