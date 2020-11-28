#Global settings
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
library(VIM)
library(ggplot2)
library(plotly)
library(shiny)
library(shinydashboard)
library(reshape2)
library(DT)
library(shinycssloaders)
library(readxl)
library(scales)

rm(list = ls())

#setwd("~/Previne/shinyapp")

#options(java.parameters = "-Xss2560k")

##0. Load data####
#Previne_19_20 <- readRDS("data/Previne_19_20_analysis.RDS")
# 
# #Previne_19_20 <- readRDS(here("Previne/data/rds/Previne_19_20_analysis.RDS"))
# 
# ###1. ANNUAL DATASET####
# 
# Previne_19_20_short <- Previne_19_20[, -c(3,5,7,8,14,35,47,56,57,58,69,70:73)]
# Previne_19_20_short$IBGE_NO_UF <- Previne_19_20$IBGE_NO_UF
# 
# #a <- as.data.frame(t(as.data.frame(lapply(Previne_19_20_short, class))))
# #a_n <- as.data.frame(names(Previne_19_20_short))
# 
# sum_    <- function(...) sum(..., na.rm = T)
# mean_ <- function(...) mean(..., na.rm = T)
# 
# 
# tic()
# Previne_a_2 <- Previne_19_20_short %>%
#   group_by(UF,NO_MUNICIPIO,IBGE_NO_G_Reg,IBGE_Hierarquia_Urbana,
#            IBGE_NO_G_Reg_Rural,IBGE_Amazônia_Legal,IBGE_Semiárido,
#            PIB.Atividade_maiorvalor, Tipologia_C_50, Porte_Mun, IBGE_NO_UF) %>%
#   summarise(
#     SIH_CID_MORTE = sum_(SIH_CID_MORTE),
#     SIH_total_AIHs = sum_(SIH_total_AIHs),
#     CNES_TP_ENVIA_y19 = sum_(CNES_TP_ENVIA_y19),
#     CNES_Qtd_ESFs_y19 = sum_(CNES_Qtd_ESFs_y19),
#     SIH_IDENT_y19 = sum_(SIH_IDENT_y19),
#     SIH_CID_MORTE_y19 = sum_(SIH_CID_MORTE_y19),
#     SIH_total_AIHs_y19 = sum_(SIH_total_AIHs_y19),
#     Total_2020 = sum_(FNS_Vltotal),
#     FNS_62060 = sum_(FNS_62060),
#     FNS_62458 = sum_(FNS_62458),
#     FNS_64758 = sum_(FNS_64758),
#     FNS_65058 = sum_(FNS_65058),
#     FNS_65578 = sum_(FNS_65578),
#     FNS_65580 = sum_(FNS_65580),
#     FNS_65582 = sum_(FNS_65582),
#     FNS_65584 = sum_(FNS_65584),
#     FNS_65586 = sum_(FNS_65586),
#     FNS_65588 = sum_(FNS_65588),
#     CNES_Qtd_UBS_Hrs_ate_59 = mean_(CNES_Qtd_UBS_Hrs_ate_59),
#     CNES_Qtd_UBS_Hrs_75 = mean_(CNES_Qtd_UBS_Hrs_75),
#     CNES_Hrs_75 = mean_(CNES_Hrs_75),
#     CNES_APS_Qtd_Medicos = mean_(CNES_APS_Qtd_Medicos),
#     CNES_APS_Qtd_Enfermeirx = mean_(CNES_APS_Qtd_Enfermeirx),
#     CNES_APS_Qtd_ProfBucal = mean_(CNES_APS_Qtd_ProfBucal),
#     SISAB_Atendimento_Individual = sum_(SISAB_Atendimento_Individual),
#     SISAB_Atendimento_Odontológico = sum_(SISAB_Atendimento_Odontológico),
#     SISAB_Procedimento = sum_(SISAB_Procedimento),
#     SISAB_Visita_Domiciliar = sum_(SISAB_Visita_Domiciliar),
#     SIH_valor_total_AIHS = sum_(SIH_valor_total_AIHS, na.rm = T),
#     SIH_total_ACSC = sum_(SIH_total_ACSC, na.rm = T),
#     SIH_valot_total_ACSC = sum_(SIH_valot_total_ACSC),
#     SIH_pct_valor_ACSC_Total = sum_(SIH_pct_valor_ACSC_Total),
#     SIH_pct_qtd_ACSC_Total = sum_(SIH_pct_qtd_ACSC_Total),
#     COVID_populacaoTCU2019 = mean_(COVID_populacaoTCU2019),
#     COVID_casosAcumulado = sum_(COVID_casosAcumulado),
#     COVID_casos_diff_mes = sum_(COVID_casos_diff_mes),
#     COVID_obitosAcumulado = sum_(COVID_obitosAcumulado),
#     COVID_obitos_dif_mes = sum_(COVID_obitos_dif_mes),
#     Total_2019 = sum_(FNS_Vltotal_y19),
#     FNS_62059_y19 = sum_(FNS_62059_y19),
#     FNS_62060_y19 = sum_(FNS_62060_y19),
#     FNS_62061_y19 = sum_(FNS_62061_y19),
#     FNS_62062_y19 = sum_(FNS_62062_y19),
#     FNS_61323_y19 = sum_(FNS_61323_y19),
#     FNS_62064_y19 = sum_(FNS_62064_y19),
#     FNS_62458_y19 = sum_(FNS_62458_y19),
#     FNS_64758_y19 = sum_(FNS_64758_y19),
#     FNS_65058_y19 = sum_(FNS_65058_y19),
#     FNS_62746_y19 = sum_(FNS_62746_y19),
#     FNS_65582_y19 = sum_(FNS_65582_y19),
#     FNS_65178_y19 = sum_(FNS_65178_y19),
#     CNES_Qtd_UBS_Hrs_ate_59_y19 = mean_(CNES_Qtd_UBS_Hrs_ate_59_y19),
#     CNES_Qtd_UBS_Hrs_75_y19 = mean_(CNES_Qtd_UBS_Hrs_75_y19),
#     CNES_Hrs_75_y19 = mean_(CNES_Hrs_75_y19),
#     CNES_APS_Qtd_Medicos_y19 = mean_(CNES_APS_Qtd_Medicos_y19),
#     CNES_APS_Qtd_Enfermeirx_y19 = mean_(CNES_APS_Qtd_Enfermeirx_y19),
#     CNES_APS_Qtd_ProfBucal_y19 = mean_(CNES_APS_Qtd_ProfBucal_y19),
#     SISAB_Atendimento_Individual_y19 = sum_(SISAB_Atendimento_Individual_y19),
#     SISAB_Atendimento_Odontológico_y19 = sum_(SISAB_Atendimento_Odontológico_y19),
#     SISAB_Procedimento_y19 = sum_(SISAB_Procedimento_y19),
#     SISAB_Visita_Domiciliar_y19 = sum_(SISAB_Visita_Domiciliar_y19),
#     SIH_valor_total_AIHS_y19 = sum_(SIH_valor_total_AIHS_y19),
#     SIH_valot_total_ACSC_y19 = sum_(SIH_valot_total_ACSC_y19),
#     SIH_pct_valor_ACSC_Total_y19 = sum_(SIH_pct_valor_ACSC_Total_y19),
#     SIH_pct_qtd_ACSC_Total_y19 = sum_(SIH_pct_qtd_ACSC_Total_y19),
#     CO.UF = max(CO.UF),
#     CO_MUNICIPIO = max(CO_MUNICIPIO),
#     IBGE_CO_G_Reg = max(IBGE_CO_G_Reg),
#     IBGE_CO_G_Reg_Rural = mean(IBGE_CO_G_Reg_Rural),
#     PIB = mean(PIB),
#     PIB.Capita = mean(PIB.Capita),
#     capital = mean(capital),
#     US_1.9 = mean(US_1.9),
#     US_3.2 = mean(US_3.2),
#     US_5.5 = mean(US_5.5),
#     Cadastro_total_Q3yr19 = mean(Cadastro_total_Q3yr19, na.rm = T),
#     cadastro_0a5_Q3yr19 = mean(cadastro_0a5_Q3yr19, na.rm = T),
#     cadastro_65mais_Q3yr19 = mean(cadastro_65mais_Q3yr19, na.rm = T),
#     cadastro_INSS_Q3yr19 = mean(cadastro_INSS_Q3yr19, na.rm = T),
#     cadastro_BPC_Q3yr19 = mean(cadastro_BPC_Q3yr19, na.rm = T),
#     cadastro_BF_Q3yr19 = mean(cadastro_BF_Q3yr19, na.rm = T),
#     SISAB_C_POP.MUN = mean(SISAB_C_QT_POPULACAO, na.rm = T),
#     SISAB_C_POP.MUN_y19 = mean(SISAB_C_QT_POPULACAO_y19, na.rm = T),
#     pop_estimada_2018 = mean(pop_estimada_2018, na.rm = T),
#     cadastro_total_pct = mean(cadastro_total_pct, na.rm = T),
#     SISAB_C_QT_EQUIPE_SF = mean_(SISAB_C_QT_EQUIPE_SF),
#     SISAB_C_QT_EQUIPE_SF_y19 = mean_(SISAB_C_QT_EQUIPE_SF_y19),
#     SISAB_C_QT_CH_MEDICO = mean_(SISAB_C_QT_CH_MEDICO),
#     SISAB_C_QT_CH_MEDICO_y19 = mean_(SISAB_C_QT_CH_MEDICO_y19),
#     SISAB_C_QT_CH_ENFERMEIRO = mean_(SISAB_C_QT_CH_ENFERMEIRO),
#     SISAB_C_QT_CH_ENFERMEIRO_y19 = mean_(SISAB_C_QT_CH_ENFERMEIRO_y19),
#     SISAB_C_QT_EQUIPE_AB_PARAMETRIZADA = mean_(SISAB_C_QT_EQUIPE_AB_PARAMETRIZADA),
#     SISAB_C_QT_EQUIPE_AB_PARAMETRIZADA_y19 = mean_(SISAB_C_QT_EQUIPE_AB_PARAMETRIZADA_y19),
#     SISAB_C_PCT_COBERTURA_SF = mean_(SISAB_C_PCT_COBERTURA_SF),
#     SISAB_C_PCT_COBERTURA_SF_y19 = mean_(SISAB_C_PCT_COBERTURA_SF_y19)
#     ) #end
# 
# toc()
# 
# # #2. Define groups####
# Previne_a_2$dif <- Previne_a_2$Total_2020 - Previne_a_2$Total_2019
# Previne_a_2$Winners_Losers <- ifelse(Previne_a_2$dif > 0, 1, 0)
# Previne_a_2$dif_relative <- round((Previne_a_2$Total_2020 - Previne_a_2$Total_2019) / Previne_a_2$Total_2019,3)
# 
# Previne_a_2$group <- ifelse(Previne_a_2$dif_relative > 0, "Acrescimo", "Decrescimo")
# Previne_a_2$group <- ifelse(Previne_a_2$dif_relative > 0.5, "Grande Acrescimo", Previne_a_2$group)
# 
# Previne_a_2$group <- ifelse(Previne_a_2$dif_relative < -0.5, "Grande Decrescimo", Previne_a_2$group)
# 
# Previne_a_2$group <- as.factor(Previne_a_2$group)
# 
# Previne_a_2$group <- relevel(Previne_a_2$group, "Grande Decrescimo")
# Previne_a_2$group <- relevel(Previne_a_2$group, "Decrescimo")
# Previne_a_2$group <- relevel(Previne_a_2$group, "Acrescimo")
# Previne_a_2$group <- relevel(Previne_a_2$group, "Grande Acrescimo")
# 
# ##2.1 Ganhos e perdas per capita####
# Previne_a_2$dif_percapita <- Previne_a_2$dif/Previne_a_2$SISAB_C_POP.MUN
# 
# P_names <- as.data.frame(names(Previne_19_20))
# 
# Previne_a_2$Recebe_PerCapita_de_Trans <- ifelse(Previne_a_2$FNS_65584 > 0, "1", "0")
# Previne_a_2$Recebe_Fator_Comp_de_Trans <- ifelse(Previne_a_2$FNS_65586 > 0, "1", "0")
# 
#  #2.2 Sensibilidade Fatores de Transição####
# Previne_a_2$Total2020_sem_transicao <- ifelse(Previne_a_2$Recebe_Fator_Comp_de_Trans == 1,
#                                               Previne_a_2$Total_2020 - Previne_a_2$FNS_65586,
#                                               Previne_a_2$Total_2020 - Previne_a_2$FNS_65584)
# 
# Previne_a_2$dif_sem_transicao <- Previne_a_2$Total2020_sem_transicao - Previne_a_2$Total_2019
# Previne_a_2$dif_relative_sem_transicao <- round((Previne_a_2$Total2020_sem_transicao - Previne_a_2$Total_2019) / Previne_a_2$Total_2019,3)
# 
# Previne_a_2$group_sem_transicao <- cut(Previne_a_2$dif_relative_sem_transicao,
#                                        breaks = c(-Inf,-.5,0,.5,Inf),
#                                        labels = c("Grande Decrescimo","Decrescimo",
#                                                   "Acrescimo", "Grande Acrescimo"))
# 
# Previne_a_2$group_sem_transicao <- as.factor(Previne_a_2$group_sem_transicao)
# Previne_a_2$group_sem_transicao <- relevel(Previne_a_2$group_sem_transicao, "Grande Decrescimo")
# Previne_a_2$group_sem_transicao <- relevel(Previne_a_2$group_sem_transicao, "Decrescimo")
# Previne_a_2$group_sem_transicao <- relevel(Previne_a_2$group_sem_transicao, "Acrescimo")
# Previne_a_2$group_sem_transicao <- relevel(Previne_a_2$group_sem_transicao, "Grande Acrescimo")
# Previne_a_2$dif_percapita_sem_transicao <- Previne_a_2$dif_sem_transicao/Previne_a_2$SISAB_C_POP.MUN
# 
#  ##3. Prop by Region####
# Prev_an_region <- Previne_a_2 %>% group_by(IBGE_NO_G_Reg,group) %>%
#   summarise(n = n())
# 
# Prev_an_region_t <- Previne_a_2 %>% group_by(IBGE_NO_G_Reg) %>% summarise(n())
# 
# Prev_an_region <- left_join(Prev_an_region,Prev_an_region_t, by = "IBGE_NO_G_Reg")
# 
# Prev_an_region$rg_prop <- Prev_an_region$n / Prev_an_region$`n()`
# Prev_an_region$rg_prop <- round(Prev_an_region$rg_prop, 3)
# 
# Prev_an_region2 <- Prev_an_region[,c(1,2,5)]
# 
# Previne_a_2 <- left_join(Previne_a_2,Prev_an_region2, by = c("IBGE_NO_G_Reg",
#                                                               "group"))
# rm(Prev_an_region2, Prev_an_region)
# 
# P_names_A <- as.data.frame(names(Previne_a_2))
# 
# #f <- ggplot(Previne_a_2, aes(x = Porte_Mun, y = cadastro_total_pct)) + geom_boxplot()
# 
# ##4. Prop by Porte####
# Prev_an_porte <- Previne_a_2 %>% group_by(Porte_Mun,group) %>%
#   summarise(n = n())
# 
# Prev_an_porte_t <- Previne_a_2 %>% group_by(Porte_Mun) %>% summarise(n())
# 
# Prev_an_porte <- left_join(Prev_an_porte,Prev_an_porte_t, by = "Porte_Mun")
# 
# Prev_an_porte$porte_prop <- Prev_an_porte$n / Prev_an_porte$`n()`
# Prev_an_porte$porte_prop <- round(Prev_an_porte$porte_prop, 3)
# 
# Prev_an_region2 <- Prev_an_porte[,c(1,2,5)]
# 
# Previne_a_2 <- left_join(Previne_a_2,Prev_an_region2, by = c("Porte_Mun",
#                                                              "group"))
# rm(Prev_an_porte_t, Prev_an_porte)
# 
# #h <- ggplot(Previne_a_2, aes(x = Porte_Mun, y = cadastro_total_pct)) + geom_boxplot()
# 
# ##5. Saverds####
# saveRDS(Previne_a_2, "data/Previne_analysis_by_year.RDS")

##5.1 (start here) LOAD RDS#####

Previne_a_2 <- readRDS("data/Previne_analysis_by_year.RDS")
Previne_a_2 <- Previne_a_2 %>% mutate_if(is.numeric, round, 3)
Previne_a_2$dif_percapita <- round(Previne_a_2$dif_percapita,2)
Previne_a_2$dif <- round(Previne_a_2$dif,2)
Previne_a_2$Tipologia_C_50 <- gsub("IntermediárioAdjacente","IntermediarioAdjacente",
                                   Previne_a_2$Tipologia_C_50)

##6. Boxplots#### 

cober_cols <- names(Previne_a_2[,c(1,2,3,9,10,87:94,96:110)])

 Boxplot_cobertura_Cadastro2 <- Previne_a_2 %>%
  select(all_of(cober_cols)) %>%
    group_by(UF, NO_MUNICIPIO, IBGE_NO_G_Reg, Tipologia_C_50, Porte_Mun) %>%
    pivot_longer(cols = c(cadastro_total_pct, SISAB_C_PCT_COBERTURA_SF), names_to = "Cadastro_Cobertura") 


##7. Analise dos Repasses####
 # Repasses <- readRDS("data/Repasses_long.RDS")
 # 
 # Repasses$date <- paste0("28/",Repasses$Compt_mes, "/", Repasses$Compt_ano)
 # Repasses$date <- dmy(Repasses$date)
 # Repasses$acao <- gsub("61323", "PAB Fixo", Repasses$acao)
 # 
 # Repasses$acao <- gsub("65178", "IMPLEMENTAÇÃO DE POLÍTICAS PARA A REDE CEGONHA", Repasses$acao)
 # 
 # #Nome das açoes
 # Cat_Cod_Acao <- read_excel("data/Previne_Dicionario_23.10.2020.xlsx", sheet = "Cat_Codigo_Acao")
 # colnames(Cat_Cod_Acao)[3] <- "Codigo"
 # Cat_Cod_Acao$Codigo <- as.character(Cat_Cod_Acao$Codigo)
 # 
 # Repasses <- left_join(Repasses, Cat_Cod_Acao, by = "Codigo")
 # Repasses$Vlacao <- Repasses$Vltotal * Repasses$pctAcao
 # 
 # Reps <- Repasses[Repasses$Categoria != "Provimento",]
 # Reps <- Reps[Reps$date != "2020-09-28",]
 # Reps <- Reps[Reps$acao.y != "INCREMENTO TEMPORARIO AO CUSTEIO DOS SERVICOS DE ABS",]
 # showonlyonce <- Reps[Reps$date == "2020-01-28" | Reps$date == "2019-01-28",]
 # 
 # showonlyonce$acao.y <- gsub("CUSTEIO DE ATENCAO A SAUDE BUCAL", "Saúde Bucal", showonlyonce$acao.y)
 # 
 # showonlyonce$acao.y <- gsub("INCENTIVO FINANCEIRO DA APS - ", " ", showonlyonce$acao.y)
 # 
 # vartoreps <- c("UF","IBGE_NO_UF", "NO_MUNICIPIO","IBGE_NO_G_Reg", "Tipologia_C_50", "capital","group", "SISAB_C_POP.MUN", "Recebe_Fator_Comp_de_Trans", "Recebe_PerCapita_de_Trans")
 # 
 # Pr_to_reps <- Previne_a_2[,vartoreps]
 # 
 # colnames(Reps)[1] <- "UF"
 # colnames(Reps)[2] <- "NO_MUNICIPIO"
 # Reps$NO_MUNICIPIO <- gsub("MUQUEM DE SAO FRANCISCO",
 #                                     "MUQUEM DO SAO FRANCISCO", 
 #                                     Reps$NO_MUNICIPIO)
 # Reps$NO_MUNICIPIO <- gsub("SANTA TERESINHA",
 #                                     "SANTA TERESINHA", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps$UF <- ifelse(Reps$NO_MUNICIPIO == "SANTA TERESINHA",
 #                             paste0("PB"), Reps$UF)
 # Reps$NO_MUNICIPIO <- gsub("ITAPAGE",
 #                                     "ITAPAJE", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps$NO_MUNICIPIO <- gsub("PASSA-VINTE",
 #                                     "PASSA VINTE", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps$NO_MUNICIPIO <- gsub("PINGO-D'AGUA",
 #                                     "PINGO D'AGUA", 
 #                                     Reps$NO_MUNICIPIO)
 # Reps$NO_MUNICIPIO <- gsub("POXOREO",
 #                                     "POXOREU", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps$NO_MUNICIPIO <- gsub("ELDORADO DOS CARAJAS",
 #                                     "ELDORADO DO CARAJAS", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps$NO_MUNICIPIO <- gsub("BELEM DE SAO FRANCISCO",
 #                                     "BELEM DO SAO FRANCISCO", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps$NO_MUNICIPIO <- gsub("OLHO-D'AGUA DO BORGES",
 #                                     "OLHO D'AGUA DO BORGES", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps$NO_MUNICIPIO <- gsub("BIRITIBA-MIRIM",
 #                                     "BIRITIBA MIRIM", 
 #                                     Reps$NO_MUNICIPIO)
 # Reps$NO_MUNICIPIO <- gsub("FLORINIA",
 #                                     "FLORINEA", 
 #                                     Reps$NO_MUNICIPIO)
 # Reps$NO_MUNICIPIO <- gsub("SAO LUIS DO PARAITINGA",
 #                                     "SAO LUIZ DO PARAITINGA", 
 #                                     Reps$NO_MUNICIPIO)
 # 
 # Reps2 <- left_join(Reps,Pr_to_reps, by = c("UF", "NO_MUNICIPIO"))
 #  rm(Repasses, Cat_Cod_Acao)
 #7.1 SaveRDS Repasses####
  #saveRDS(Reps2, "data/Repasses_previne.RDS")
 
 #7.2 Load RepassesPrevine.RDS####
 Repasses_Previne <- readRDS("data/Repasses_previne.RDS")
 
 Repasses_Previne$Categoria <-  ifelse(Repasses_Previne$acao.y == "PAB VARIAVEL", "PAB VARIAVEL",
                                       Repasses_Previne$Categoria) 
 
 addUnits <- function(n) {
   labels <- ifelse(n < 1000, n,  # less than thousands
                    ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                           ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                  ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                         ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                                'too big!'
                                         )))))
   return(labels)
 }

# 8. Select Axis Options items####
axis_vars <- c(
  "UF" = "UF",
  "Principal Atividade PIB" = "PIB.Atividade_maiorvalor",
  "Região" = "IBGE_NO_G_Reg",
  "Porte do Mun." = "Porte_Mun",
  "Tipologia" = "Tipologia_C_50")

axis_vars_numeric <- c(

  "Casos de Covid (Aug 2020)" = "COVID_casosAcumulado",
  "Diferença no repasse recebido (2020-2019)" = "dif",
  "Diferença no repasse recebido per capita" = "dif_percapita",
  "N. total de internações (AIHs)" = "SIH_total_AIHs",
  "N. total de internações (AIHs) (2019)" = "SIH_total_AIHs_y19",
  "Obitos de Covid (Aug 2020)" = "COVID_obitosAcumulado",
  "PIB (2017)" = "PIB",
  "PIB per Capita (2017)" = "PIB.Capita",
  "Prop. de Internações por condições sensíveis APS" = "SIH_pct_qtd_ACSC_Total",
  "Prop. de Internações por condições sensíveis APS (2019)" = "SIH_pct_qtd_ACSC_Total_y19",
  "Prop. Pessoas US$1.9/dia" = "US_1.9",
  "Prop. Pessoas US$3.2/dia" = "US_3.2",
  "Prop. Pessoas US$5.5/dia" = "US_5.5",
  "Prop. Pessoas cadastradas" = "cadastro_total_pct",
  "Prop. Pessoas cobertas pelas eSFs" = "SISAB_C_PCT_COBERTURA_SF",
  "Qtd de equipes (eSF)" = "SISAB_C_QT_EQUIPE_SF",
  "Qtd de Medicos" = "CNES_APS_Qtd_Medicos",
  "Qtd de Medicos (2019)" = "CNES_APS_Qtd_Medicos_y19",
  "Qtd. de Atendimentos Individuais (APS)" = "SISAB_Atendimento_Individual",
  "Qtd. de Atendimentos Individuais (APS) (2019)" = "SISAB_Atendimento_Individual_y19",
  "Qtd. de Visitas Dom." = "SISAB_Visita_Domiciliar",
  "Qtd. de Visitas Dom. (2019)" = "SISAB_Visita_Domiciliar_y19",
  "Taxa de mudança" = "dif_relative",
  "Total de cadastros (Q32019)" = "Cadastro_total_Q3yr19",
  "Total de cadastros Bolsa Familia (Q32019)" = "cadastro_BF_Q3yr19",
  "Valor Total de Internações por condições sensíveis APS" = "SIH_pct_valor_ACSC_Total",
  "Valor Total de Internações por condições sensíveis APS (2019)" = "SIH_valot_total_ACSC_y19")

###9. Dicionario####

Previne_Dicionario <- read_excel("data/Previne_Dicionario_23.10.2020.xlsx", skip = 1)
Previne_Dicionario <- Previne_Dicionario[,1:6]
p_a_names <- as.data.frame(names(Previne_a_2))


