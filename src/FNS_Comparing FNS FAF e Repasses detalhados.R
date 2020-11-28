#Investigating other FNS all repasses file


library(dplyr)
library(here)
library(tidyr)
library(ggplot2)
library(plotly)
library(data.table)

setwd(here("Previne"))

list.files(here("Previne/data/raw/REPASSE_FAF_COM_POPULAÇÃO_-_2020_-_acumulado_até_03092020_8920"))
fns_repasses_one <- read.csv(here("Previne/data/raw/REPASSE_FAF_2020_acumulado_até_03092020_8920/REPASSE_FAF_COM_POP_2020_acumulado_ate_03092020.csv"))

fns <- readRDS(here("Previne/data/rds/fns_transfers_data.RDS"))

fns_2979 <- FNS_APS_JAN_SET_2020 %>%
  filter(nuPortaria == 2979)

##Download arquivo unico####
ff <- fns_repasses_one %>%
  separate(DT_SALDO_CONTA, c("dia", "mes", "ano"), sep = "/")

ff$VL_BRUTO <- as.numeric(gsub(",", ".", ff$VL_BRUTO)  )
ff$VL_LIQUIDO <- as.numeric(gsub(",", ".", ff$VL_LIQUIDO))
                    

f_2020 <- ff %>%
  filter(GRUPO == "ATENÇÃO BÁSICA") %>%
  group_by(UF,mes, MUNICIPIO, ESTRATÉGIA) %>%
  summarise(vl.bruto = sum(VL_BRUTO), vl.liq = sum(VL_LIQUIDO))

f_2020_t <- ff %>%
  filter(GRUPO == "ATENÇÃO BÁSICA") %>%
  group_by(UF,mes, MUNICIPIO) %>%
  summarise(vl.bruto = sum(VL_BRUTO), vl.liq = sum(VL_LIQUIDO))

f_2020_r <- left_join(f_2020,f_2020_t, by = c("UF", "mes", "MUNICIPIO") )

f_2020_r$pctAcao <- f_2020_r$vl.bruto.x/f_2020_r$vl.bruto.y

###Comparing###

fw_2020 <- f_2020_r %>%
  filter(MUNICIPIO == "ACRELANDIA")
fs_2020 <- fns %>%
  filter(noMunicipio == "ACRELANDIA")
