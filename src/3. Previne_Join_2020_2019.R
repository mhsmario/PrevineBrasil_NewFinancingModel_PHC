#Join 20+19 Previne Datasets#####
#Putting together Previne 2019 and 2020 datasets for analysis

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
library(ggplot2)
library(readxl)
#library(factoextra)
#set_here("C:/Users/wb441213/WBG/Edson Correia Araujo - Financiamento APS/Previne Impacts")

setwd("C:/Users/wb441213/WBG/Edson Correia Araujo - Financiamento APS/Previne Impacts")

#0. Setup ####
rm(list = ls())
gc()

Previne_19 <- readRDS("Data Analysis/data/rds/Previne_dataset_2019.RDS")
nas_19 <- as.data.frame(sapply(Previne_19, function(x) sum(is.na(x))))
colnames(nas_19)[1] <- "Missing"
nas_19$obs <- nrow(Previne_19) - nas_19$Missing 
nas_19$pct_nas <- nas_19$Missing/nrow(Previne_19)

Previne_20 <- readRDS("Data Analysis/data/rds/Previne_dataset_2020.RDS")
nas_20 <- as.data.frame(sapply(Previne_20, function(x) sum(is.na(x))))
colnames(nas_20)[1] <- "Missing"
nas_20$obs <- nrow(Previne_20) - nas_20$Missing 
nas_20$pct_nas <- nas_20$Missing/nrow(Previne_20)

rm(nas_19, nas_20)
Previne_19_short <- Previne_19[,c(1,4,6,8,16,17,18,19,21:35,38:56)]

n19 <- names(Previne_19_short)
n19 <- paste0(n19, "_y19")
colnames(Previne_19_short) <- n19

colnames(Previne_19_short)[1] <- "CO.UF"
colnames(Previne_19_short)[2] <- "CO_MUNICIPIO"
colnames(Previne_19_short)[4] <- "Mes"

Previne_20_19 <- left_join(Previne_20,Previne_19_short, by = c("CO_MUNICIPIO", "Mes") ) 
rm(Previne_19_short, n19, Previne_20, Previne_19)

Previne_20_19$CO_MUNICIPIO_m2 <- ifelse(is.na(Previne_20_19$CO_MUNICIPIO_m2), 
                                        Previne_20_19$CO_MUNICIPIO,
                                        Previne_20_19$CO_MUNICIPIO_m2)

Previne_20_19$CO_MUNICIPIO_m2 <- ifelse(nchar(Previne_20_19$CO_MUNICIPIO_m2) > 6,
                                        str_sub(Previne_20_19$CO_MUNICIPIO_m2, end = 6),
                                        Previne_20_19$CO_MUNICIPIO_m2)

#Previne_20_19 <- readRDS(here("Previne/data/rds/Previne_19_20_analysis.RDS"))

##Fix Before saving
colnames(Previne_20_19)[1] <- "CO.UF"
colnames(Previne_20_19)[3] <- "IBGE_NO_UF"
colnames(Previne_20_19)[9] <- "IBGE_CO_G_Reg"
colnames(Previne_20_19)[10] <- "IBGE_NO_G_Reg"
colnames(Previne_20_19)[12] <- "IBGE_CO_G_Reg_Rural"
colnames(Previne_20_19)[13] <- "IBGE_NO_G_Reg_Rural"

names <- colnames(Previne_20_19)
names <- gsub(" ", "_", names)
colnames(Previne_20_19) <- names
#Fix State abreviations
uf <- Previne_20_19[,c("CO.UF", "UF")]
uf <- unique(uf) 
uf <- filter(uf, !is.na(uf$UF))
Previne_20_19 <- left_join(Previne_20_19, uf, by = "CO.UF")
Previne_20_19$UF.x <- Previne_20_19$UF.y
Previne_20_19 <- Previne_20_19[,-109]

mun <- Previne_20_19[,c("CO_MUNICIPIO_m2", "CO.UF", "NO_MUNICIPIO")]
mun <- unique(mun) 
mun <- filter(mun, !is.na(CO_MUNICIPIO_m2))

Previne_20_19 <- left_join(Previne_20_19, mun, by = c("CO.UF","NO_MUNICIPIO"))
Previne_20_19$CO_MUNICIPIO_m2.x <- Previne_20_19$CO_MUNICIPIO_m2.y
Previne_20_19 <- Previne_20_19[,-c(109)]

#Rename variables
colnames(Previne_20_19)[2] <- "UF"
colnames(Previne_20_19)[17] <- "PIB" 
colnames(Previne_20_19)[18] <- "PIB.Capita"
colnames(Previne_20_19)[19] <- "PIB.Atividade_maiorvalor"

colnames(Previne_20_19)[71] <- "PIB_y19" 
colnames(Previne_20_19)[72] <- "PIB.Capita_y19"
colnames(Previne_20_19)[73] <- "PIB.Atividade_maiorvalor_y19"

colnames(Previne_20_19)[66] <- "US_1.9" 
colnames(Previne_20_19)[67] <- "US_3.2" 
colnames(Previne_20_19)[68] <- "US_5.5" 

colnames(Previne_20_19)[38] <- "CNES_Qtd_UBS_Hrs_75" 
colnames(Previne_20_19)[92] <- "CNES_Qtd_UBS_Hrs_75_y19"

###0.1 Recalculate transfers####

convertFNSpct_20 <- function(x, na.rm = FALSE) 
  (x * Previne_20_19$FNS_Vltotal)

convertFNSpct_19 <- function(x, na.rm = FALSE) 
  (x * Previne_20_19$FNS_Vltotal_y19)

fns_2020 <- names(Previne_20_19[,c(21:30)])

for (i in 21:30) { 
Previne_20_19[,i] <- convertFNSpct_20(Previne_20_19[,i]) 
} 
fns_2019 <- names(Previne_20_19[,c(75:86)])

for (i in 75:86) { 
  Previne_20_19$i <- convertFNSpct_19(i) }

###1. SISAB - Cobertura Until Aug 2020 ####
SISAB_Cobertura <- readRDS("Data Analysis/data/rds/SISAB_Cobertura.RDS")

colnames(SISAB_Cobertura)[2] <- "CO_MUNICIPIO_m2.x"


##Merge with Cobertura
Previne_Coertura_20_19 <- left_join(Previne_20_19, SISAB_Cobertura, by = c("UF", "CO_MUNICIPIO_m2.x", "Mes") )
sapply(Previne_Coertura_20_19, function(x) sum(is.na(x)))
merge1_nas <- Previne_Coertura_20_19 %>% filter(is.na(SISAB_C_QT_POPULACAO))
merge1_nas <- unique(merge1_nas$NO_MUNICIPIO)

colnames(Previne_Coertura_20_19)[5] <- "CO_MUNICIPIO_m2"

##2. Tipologia, Cadastros e Vulneraveis ####
Tipologia_e_Cadastro_Q3_2019 <- read_excel("Data Analysis/data/csv_excel/Tipologia_e_Cadastro_Q3_2019.xlsx")

colnames(Tipologia_e_Cadastro_Q3_2019)[1] <- "CO_MUNICIPIO_m2"
Tipologia_e_Cadastro_Q3_2019 <- Tipologia_e_Cadastro_Q3_2019[,-c(2,3,5)]

Tipologia_e_Cadastro_Q3_2019$CO_MUNICIPIO_m2 <- as.character(Tipologia_e_Cadastro_Q3_2019$CO_MUNICIPIO_m2)
Previne_Coertura_Tipologia_20_19 <- left_join(Previne_Coertura_20_19, Tipologia_e_Cadastro_Q3_2019, by = c("CO_MUNICIPIO_m2") )

colnames(Previne_Coertura_Tipologia_20_19)[127] <- "Tipologia_C_50"
colnames(Previne_Coertura_Tipologia_20_19)[128] <- "Cadastro_total_Q3yr19"
colnames(Previne_Coertura_Tipologia_20_19)[129] <- "cadastro_0a5_Q3yr19"
colnames(Previne_Coertura_Tipologia_20_19)[130] <- "cadastro_65mais_Q3yr19"
colnames(Previne_Coertura_Tipologia_20_19)[131] <- "cadastro_INSS_Q3yr19"
colnames(Previne_Coertura_Tipologia_20_19)[132] <- "cadastro_BPC_Q3yr19"
colnames(Previne_Coertura_Tipologia_20_19)[133] <- "cadastro_BF_Q3yr19"

Previne_Coertura_Tipologia_20_19 <- Previne_Coertura_Tipologia_20_19[,-134]

Previne_Coertura_Tipologia_20_19$Tipologia_C_50 <- gsub(" ", "", Previne_Coertura_Tipologia_20_19$Tipologia_C_50)
Previne_Coertura_Tipologia_20_19$Tipologia_C_50 <- gsub("Intermedi?rio", "Intermediario", Previne_Coertura_Tipologia_20_19$Tipologia_C_50)

colnames(Previne_Coertura_Tipologia_20_19)[8] <- "Ano"
Previne_Coertura_Tipologia_20_19 <- Previne_Coertura_Tipologia_20_19[,-109]

# 3. Calculate Difference####

Previne_Coertura_Tipologia_20_19$Dif_FNS_Vltotal <- Previne_Coertura_Tipologia_20_19$FNS_Vltotal-Previne_Coertura_Tipologia_20_19$FNS_Vltotal_y19

##4. Classify Population size of Muncipalities####

Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO <- Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO * 1000

Sis <- SISAB_Cobertura %>% group_by(CO_MUNICIPIO_m2.x) %>%
  summarise(SISAB_C_QT_POPULACAO = mean(SISAB_C_QT_POPULACAO))

#Fixing NA's in the Population variable
for (i in 1:nrow(Previne_Coertura_Tipologia_20_19)) { 
    if (is.na(Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO[i])) {
          co_mun2 <- paste0(Previne_Coertura_Tipologia_20_19$CO_MUNICIPIO_m2[i])
          result <- as.numeric(paste0(Sis[Sis$CO_MUNICIPIO_m2.x == co_mun2,2]))
          result <- result*1000
          Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO[i] <- result
      }#if-statement
  }#loop

Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO <- ifelse(is.na(Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO), Previne_Coertura_Tipologia_20_19$pop_estimada_2018, Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO)


Previne_Coertura_Tipologia_20_19$Porte_Mun <- cut(Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO, 
                               breaks = c(-Inf, 20000, 50000, 100000, 300000, Inf), 
                               labels = c("P.Ate_20mil", "P.De20_49mil", "P.De50_100mil", "P.De100_300mil", "P.Mais_de_300mil"), 
                               right = FALSE)

tt <- Previne_Coertura_Tipologia_20_19 %>% group_by(CO_MUNICIPIO, Porte_Mun) %>%
  summarise(n())
tt$dup <- duplicated(tt$CO_MUNICIPIO)
table(tt$dup)
t <- tt[tt$dup == T,]
if (nrow(t) > 0) { 
  print("Oh no, it seems there are duplicates! Stopping now")
  break 
  }

Previne_Coertura_Tipologia_20_19$cadastro_total_pct <- Previne_Coertura_Tipologia_20_19$Cadastro_total_Q3yr19/Previne_Coertura_Tipologia_20_19$SISAB_C_QT_POPULACAO



###5. SaveRDS####
#Previne Impacts Folder
saveRDS(Previne_Coertura_Tipologia_20_19, paste0("Data Analysis/data/rds/Previne_19_20_analysis.RDS")) 


###5. Save DTA####
Previne_19_20 <- readRDS(paste0("Data Analysis/data/rds/Previne_19_20_analysis.RDS")) 

namesdf <- paste0("Var", 1:length(Previne_19_20))

colnames(Previne_19_20) <- namesdf

rio::export(Previne_19_20, "Data Analysis/data/dta/Previne_19_20_analysis.dta")
