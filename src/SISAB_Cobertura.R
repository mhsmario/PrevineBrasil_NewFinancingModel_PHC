#SISAB Cobertura

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
library(ggplot2)
library(readxl)

#source: https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/relHistoricoCoberturaAB.xhtml

read_plus <- function(flnm) {
  fread(flnm, skip = 6) %>% 
    mutate(filename = flnm)
}

SISAB_Q319_2020 <- 
  list.files(path = here("Data Analysis","data", "raw", "SISAB", "Cobertura"), 
             pattern = "*.csv",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))

SISAB_Q319_2020 <- SISAB_Q319_2020[,-c(16:27)]

SISAB_2019 <- read_excel("C:/Users/wb441213/WBG/Edson Correia Araujo - Financiamento APS/Previne Impacts/Data Analysis/data/raw/SISAB/Cobertura/Historico_AB_MUNICIPIOS_2007_2019SET.xlsx", 
                                                   sheet = "2019")


colnames(SISAB_Q319_2020) <- c("NU_COMPETENCIA", "NO_REGIAO", "SG_UF", "CO_MUNICIPIO_IBGE",
                               "NO_MUNICIPIO_ACENTUADO", "QT_POPULACAO", "QT_EQUIPE_SF",
                               "QT_EQUIPE_AB_PARAMETRIZADA", "QT_CH_MEDICO", "QT_CH_ENFERMEIRO",
                               "QT_EQUIPE_SF_AB", "QT_COBERTURA_SF", "PC_COBERTURA_SF",
                               "QT_COBERTURA_AB", "PC_COBERTURA_AB")


SISAB_2019 <- SISAB_2019[,-c(2,3,5,6,14,16,17)]

SISAB_Cobertura <- rbind(SISAB_Q319_2020, SISAB_2019)
SISAB_Cobertura <- SISAB_Cobertura[!is.na(SISAB_Cobertura$NU_COMPETENCIA),]
SISAB_Cobertura$NU_COMPETENCIA <- tolower(SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("jan", "01/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("fev", "02/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("mar", "03/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("abr", "04/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("mai", "05/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("jun", "06/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("jul", "07/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("ago", "08/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("set", "09/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("out", "10/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("nov", "11/01",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("dez", "12/01",SISAB_Cobertura$NU_COMPETENCIA)

SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201901", "01/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201902", "02/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201903", "03/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201904", "04/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201905", "05/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201906", "06/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201907", "07/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201908", "08/01/2019",SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$NU_COMPETENCIA <-  gsub("201909", "09/01/2019",SISAB_Cobertura$NU_COMPETENCIA)

SISAB_Cobertura$NU_COMPETENCIA <- mdy(SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$Mes <- month(SISAB_Cobertura$NU_COMPETENCIA)
SISAB_Cobertura$Ano <- year(SISAB_Cobertura$NU_COMPETENCIA)

SISAB_Cobertura_mun <- SISAB_Cobertura %>%
  group_by(SG_UF, CO_MUNICIPIO_IBGE, Mes, Ano) %>%
  summarise(QT_POPULACAO = mean(as.numeric(QT_POPULACAO), na.rm = T),
            QT_EQUIPE_SF = mean(as.numeric(QT_EQUIPE_SF), na.rm = T),
            QT_EQUIPE_AB_PARAMETRIZADA = mean(as.numeric(QT_EQUIPE_AB_PARAMETRIZADA), na.rm = T),
            QT_CH_MEDICO = mean(as.numeric(QT_CH_MEDICO), na.rm = T),
            QT_CH_ENFERMEIRO = mean(as.numeric(QT_CH_ENFERMEIRO), na.rm = T),
            QT_COBERTURA_SF = mean(as.numeric(QT_COBERTURA_SF), na.rm = T),
            QT_COBERTURA_AB = mean(as.numeric(QT_COBERTURA_AB), na.rm = T))


SISAB_Cobertura_mun_20 <- filter(SISAB_Cobertura_mun, Ano == 2020)

SISAB_Cobertura_mun_19 <- filter(SISAB_Cobertura_mun, Ano == 2019)
colnames(SISAB_Cobertura_mun_19)[5] <- "QT_POPULACAO_y19"
colnames(SISAB_Cobertura_mun_19)[6] <- "QT_EQUIPE_SF_y19"
colnames(SISAB_Cobertura_mun_19)[7] <- "QT_EQUIPE_AB_PARAMETRIZADA_y19"
colnames(SISAB_Cobertura_mun_19)[8] <- "QT_CH_MEDICO_y19"
colnames(SISAB_Cobertura_mun_19)[9] <- "QT_CH_ENFERMEIRO_y19"
colnames(SISAB_Cobertura_mun_19)[10] <- "QT_COBERTURA_SF_y19"
colnames(SISAB_Cobertura_mun_19)[11] <- "QT_COBERTURA_AB_y19"

rm(SISAB_Cobertura_mun, SISAB_Cobertura)

SISAB_Cobertura_mun_1920 <- left_join(SISAB_Cobertura_mun_20, SISAB_Cobertura_mun_19, by = c("SG_UF", "CO_MUNICIPIO_IBGE", "Mes"))

SISAB_Cobertura_mun_1920 <- SISAB_Cobertura_mun_1920[,-c(12)]
names_sisab_c <- colnames(SISAB_Cobertura_mun_1920)
names_sisab_c <- paste0("SISAB_C_", names_sisab_c)
colnames(SISAB_Cobertura_mun_1920) <- names_sisab_c

colnames(SISAB_Cobertura_mun_1920)[4] <- "Ano"
colnames(SISAB_Cobertura_mun_1920)[1] <- "UF"
colnames(SISAB_Cobertura_mun_1920)[2] <- "CO_MUNICIPIO"
colnames(SISAB_Cobertura_mun_1920)[3] <- "Mes"

SISAB_Cobertura_mun_1920$SISAB_C_PCT_COBERTURA_SF <- SISAB_Cobertura_mun_1920$SISAB_C_QT_COBERTURA_SF / SISAB_Cobertura_mun_1920$SISAB_C_QT_POPULACAO
 
SISAB_Cobertura_mun_1920$SISAB_C_PCT_COBERTURA_SF_y19 <- SISAB_Cobertura_mun_1920$SISAB_C_QT_COBERTURA_SF_y19 / SISAB_Cobertura_mun_1920$SISAB_C_QT_POPULACAO_y19

saveRDS(SISAB_Cobertura_mun_1920, here("Data analysis","data","rds", "SISAB_Cobertura.RDS"))

  