#Var = Internacoes evitaveis + Total numero consultas MAC
  #Source = http://www2.datasus.gov.br/DATASUS/index.php?area=0901'
    #Output file = SIH_AIH_ACSC_2019_totals.RDS
    #Output file = SIH_ACSC_AIH_2019.RDS
    #Output file = SIH_total_2019.RDS


#0. Project Info####
##PROCESSAMENTO DOS DADOS DE INTERNA��ES HOSPITALARES
##ANO = 2019

#Site que fala um pouco sobre onde encontrar os dados do SUS.
#https://dadosabertos.social/t/como-obter-e-tratar-dados-do-datasus/66

#Datasus - Interna��es Hospitalares

#Download de microdados e documenta��o da base de dados - http://www2.datasus.gov.br/DATASUS/index.php?area=0901'

#Para entender a fundo os dados coletados pelo Sistema de Info Hospitalar

#'Download do MANUAL T�CNICO DO SISTEMA DE INFORMA��O HOSPITALAR - http://bvsms.saude.gov.br/bvs/publicacoes/07_0066_M.pdf'

# Estrutura da numera��o da AIH:####
# 
# . Primeiro e segundo d�gitos correspondem � unidade da federa��o, de acordo com o c�digo do IBGE.  Exceto nos casos das s�ries num�ricas espec�ficas da Central Nacional de Regula��o de Alta Complexidade (CNRAC), que iniciam com o n�mero 99, indicando que corresponde a todo Brasil, sem divis�o por unidade federada.
# 
# . Terceiro e quarto d�gitos correspondem aos dois �ltimos algarismos do ano de refe- r�ncia (Ex.: 06 para 2006).
# 
# . O quinto d�gito identifica: a) 1 (um) que a numera��o � de (AIH) - uso geral; b) 3 (tr�s) que a numera��o � de AIH espec�fica da CNRAC; c) 5 (cinco) que a numera��o � de AIH espec�fica para os procedimentos cir�rgicos eletivos de m�dia complexidade,
# 
# . Os sete algarismos seguintes, que correspondem �s posi��es 6, 7, 8, 9, 10, 11, e 12, obedecem � ordem crescente, come�ando em 0.000.001, indo at� 9.999.999.
# 
# . O �ltimo algarismo, da posi��o 13, � o d�gito verificador, calculado pelo programa "DR SYSTEM".


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
library(read.dbc)

#install.packages("read.dbc")
#set_here("Previne")

#sus <- readdbc("RDTO1901dbc")

setwd(here("Previne"))

#1. List data files ####
myfiles <- list.files(path = here("Previne/data/raw/SIH/2019/"), "csv$")

tic("total")

#2. Prep. dimension reduction parameters ####
tokeep <- c(1:10, 23:26, 36, 37, 38, 39, 40:42, 44, 45, 49:52, 56, 58, 59, 61,62,65,66,75,78,80,84,85,86,96,97,98,105,107)

#Select COD-10 ACSC codes
ACSC_BR <- c("A00", "A00", "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A150", "A151", "A152", "A153", "A154", "A155", "A156", "A157", "A158", "A159", "A160", "A161", "A162", "A163", "A164", "A165", "A166", "A167", "A168", "A169", "A170", "A171", "A172", "A173", "A174", "A175", "A176", "A177", "A178", "A179", "A18", "A19", "A33", "A34", "A35", "A36", "A37", "A46", "A50", "A51", "A52", "A53", "A95", "B05", "B06", "B16", "B26", "B50", "B51", "B51", "B51", "B51", "B52", "B52", "B52", "B52", "B53", "B53", "B53", "B53", "B54", "B77", "D50", "E100", "E101", "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109", "E110", "E111", "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E120", "E121", "E122", "E123", "E124", "E125", "E126", "E127", "E128", "E130", "E131", "E133", "E134", "E135", "E136", "E137", "E138", "E139", "E140", "E141", "E143", "E144", "E145", "E146", "E147", "E148", "E40", "E41", "E42", "E43", "E44", "E45", "E46", "E50", "E51", "E52", "E53", "E54", "E55", "E56", "E57", "E58", "E59", "E60", "E61", "E63", "E63", "E64", "E86", "G000", "G40", "G41", "G45", "G46", "H66", "I00", "I02", "I10", "I11", "I20", "I50", "I63", "I64", "I65", "I66", "I67", "I69", "J00", "J01", "J02", "J03", "J06", "J13", "J14", "J153", "J154", "J158", "J159", "J181", "J20", "J21", "J31", "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J81", "K25", "K26", "K27", "K28", "K920", "K921", "K922", "L01", "L02", "L03", "L04", "L08", "N10", "N11", "N12", "N30", "N34", "N390", "N70", "N71", "N72", "N73", "N75", "N76", "O23", "P350")


#3. Process data####
##Filter database to extract only potentially avoidable situations


#Convert all DBC to CSV####
# for (i in 1:length(myfiles)) {
#   
#   df_dbc <- read.dbc(here(paste0("data/raw/SIH/2019/dbc/", myfiles[i])))
#   write.csv(df_dbc, here(paste0("data/raw/SIH/2019/", myfiles[i], ".csv")))
#   
#   if (i == 50) {print(i/length(myfiles)*100)}
#   if (i == 100) {print(i/length(myfiles)*100)}
#   if (i == 150) {print(i/length(myfiles)*100)}
#   if (i == 200) {print(i/length(myfiles)*100)}
# }

#Get data from each table####


# read_plus <- function(flnm) {
#   fread(flnm) %>% 
#     mutate(filename = flnm) 
#   }
  

tic("loop_ACSC")
SIH_AIH_ACSC_2019 <- data.frame()
#total_obs_2019 <- data.frame()

for (i in 1:length(myfiles)) {
  
  df_dbc <- fread(here(paste0("Previne/data/raw/SIH/2019/", myfiles[i])))
  df_dbc <- df_dbc[, c(1:10, 23:26, 36,37, 38, 39, 40:42, 44, 45, 49:52, 56, 58, 59, 61,62,65,66,75,78,80,84,85,86,96,97,98,105,107)]
  df_dbc <- df_dbc[, c(2, 25,3:5,8,16,21, 37)]
  
  ##Extract total number of obs by month
  #  obs <- df_dbc %>%
  #    group_by(UF_ZI, ANO_CMPT, MES_CMPT,MUNIC_MOV, IDENT, CID_MORTE) %>%
  #    summarise(total = n(), vltotal = sum(VAL_TOT)) 
  #  obs <- as.data.frame(obs)
  # total_obs_2019 <- rbind(total_obs_2019, obs)
  
  ##Extract data - #Filter by ACSC CID10 conditions
  #df_dbc$file <- paste0(myfiles[i])

  df_dbc_ACSC <- df_dbc %>%
     filter(DIAG_PRINC %in% ACSC_BR) %>%
      group_by(UF_ZI, ANO_CMPT, MES_CMPT,MUNIC_MOV, IDENT, CID_MORTE, DIAG_PRINC) %>%
        summarise(total = n(), vltotal = sum(VAL_TOT)) 
  df_dbc_ACSC <- as.data.frame(df_dbc_ACSC)
  SIH_AIH_ACSC_2019 <- rbind(SIH_AIH_ACSC_2019, df_dbc_ACSC)
  
   if (i == 1) {tic("Iteration 1")}
   if (i == 25) {
     print(paste0(i, " iterations in:"))
      toc()
     print(paste0(round(i/length(myfiles)*100, 2), "% done"))
     }
   if (i == 50) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))}
   if (i == 75) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))
     }
   if (i == 100) {
     print(paste0(round(i/length(myfiles)*100, 2), "% done"))
     gc()
     }
  
   if (i == 125) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))}
   if (i == 150) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))}
   if (i == 175) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))}
   if (i == 200) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))}
   if (i == 250) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))}
   if (i == 300) {print(paste0(round(i/length(myfiles)*100, 2), "% done"))}
}

toc()


SIH_AIH_ACSC_2019_noDiag <- SIH_AIH_ACSC_2019 %>%
  group_by(UF_ZI, ANO_CMPT, MES_CMPT,MUNIC_MOV, IDENT, 
           CID_MORTE) %>%
  summarise(total_ACSC = n(), valot_total_ACSC = sum(vltotal)) 

colnames(total_obs_2019)[7] <- "total_AIHs"
colnames(total_obs_2019)[8] <- "valor_total_AIHS"


SIH_AIC_ACSC_2019_totals <- left_join(total_obs_2019, SIH_AIH_ACSC_2019_noDiag,
                                      by = c("UF_ZI", "ANO_CMPT", "MES_CMPT", 
                                             "MUNIC_MOV", "IDENT", "CID_MORTE"))

SIH_AIC_ACSC_2019_totals$valot_total_ACSC <- ifelse(is.na(SIH_AIC_ACSC_2019_totals$valot_total_ACSC), 0, SIH_AIC_ACSC_2019_totals$valot_total_ACSC)

SIH_AIC_ACSC_2019_totals$total_ACSC <- ifelse(is.na(SIH_AIC_ACSC_2019_totals$total_ACSC), 0, SIH_AIC_ACSC_2019_totals$total_ACSC)

SIH_AIC_ACSC_2019_totals$pct_valor_ACSC_Total <- SIH_AIC_ACSC_2019_totals$valot_total_ACSC / SIH_AIC_ACSC_2019_totals$valor_total_AIHS

SIH_AIC_ACSC_2019_totals$pct_qtd_ACSC_Total <- SIH_AIC_ACSC_2019_totals$total_ACSC / SIH_AIC_ACSC_2019_totals$total_AIHs


#Save RDS####
saveRDS(SIH_AIH_ACSC_2019, paste0(here("Previne","data", "rds"), "/SIH_ACSC_AIH_2019.RDS"))

saveRDS(total_obs_2019, file = paste0(here("Previne","data", "rds"), "/SIH_total_2019.RDS"))

saveRDS(SIH_AIC_ACSC_2019_totals, file = paste0(here("Previne","data", "rds"), "/SIH_AIH_ACSC_2019_totals.RDS"))
