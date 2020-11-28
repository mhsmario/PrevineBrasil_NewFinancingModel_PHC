library(rvest)
library(rjson)
library(tidyjson)
library(xlsx)
library(dplyr)
library(here)
library(tictoc)
library(data.table)
library(tidyr)

setwd(here("Previne"))
APS_FNS <- readRDS(paste0(here::here("Documents/WB/Health/data/FNS_URL_20_19_18.RDS")))

#APS_FNS <- APS_FNS[c(316,317,318,602:697),]

APS_FNS$url <- gsub("count=10", "count=100000", APS_FNS$url)

APS_FNS <- separate(APS_FNS, url, into = c("url", "trash"), sep = "'")

APS_FNS20 <- APS_FNS %>% filter(APS_FNS$Ano_posted == 2020)
APS_FNS <- APS_FNS20[,-2]

#TESTE <- APS_FNS[c(1:4),]

tic("start")

results_18 <- data.frame()
results_19 <- data.frame()
results_20 <- data.frame()
results_21 <- data.frame()
results_22 <- data.frame()
results_23 <- data.frame()
results_24 <- data.frame()

for (i in 1:nrow(APS_FNS)) {
  
  tic(paste0("iteration = ", i))
  
  simple <- read_html(paste0(APS_FNS$url[i]))
  
  html_txt <- simple %>%
    html_nodes("p") %>%
    html_text()
  
  json_html <- fromJSON(html_txt)
  
  df_json_html <- json_html[["resultado"]][["repasses"]] %>%
    #gather_keys()
    spread_all() 
  
  df_json_html$url <- paste0(APS_FNS$url[i])
  df_json_html$Compt_mes <- paste0(APS_FNS$Competencia_Mes[i])
  df_json_html$Compt_ano <- paste0(APS_FNS$Competencia_Ano[i])
  df_json_html$Codigo <- paste0(APS_FNS$codigo[i])
  # df_json_html$Acao <- paste0(APS_FNS$Acao[i])
  # df_json_html$Codigo.Previne <- paste0(APS_FNS$Codigo.Previne[i])
  # df_json_html$Acao.Previne <- paste0(APS_FNS$Acao.Previne[i])
  df_json_html$Prog_Emenda <- paste0(APS_FNS$Programa_ou_Emenda[i])
  df_json_html$nuMes <- paste0(APS_FNS$Mes_posted[i])
  df_json_html$nuAno <- paste0(APS_FNS$Ano_posted[i])
  
  if (ncol(df_json_html) < 17) {
    print("Number of columns less than 17")
    print(i)
  }
  
  if (ncol(df_json_html) == 18) {
    results_18 <- rbind(results_18, df_json_html)
    
  }
  
  if (ncol(df_json_html) == 19) {
    results_19 <- rbind(results_19, df_json_html)
    
  }
  if (ncol(df_json_html) == 20) {
    results_20 <- rbind(results_20, df_json_html)
    
  }
  
  if (ncol(df_json_html) == 21) {
    results_21 <- rbind(results_21, df_json_html)
    
  }
  if (ncol(df_json_html) == 22) {
    results_22 <- rbind(results_22, df_json_html)
    
  }
  
  if (ncol(df_json_html) == 23) {
    results_23 <- rbind(results_23, df_json_html)
  }
  
  if (ncol(df_json_html) > 23) {
    print("Number of columns greater than 23")
    print(i)
  }
  
  toc()
}

toc()

##Fix column names before merging dataframes
results_18$nuPortaria <- "Nao se aplica"

FNS_APS_data <- rbind(results_19, results_18)

saveRDS(FNS_APS_data, paste0(here::here("Documents", "WB","Health","data/","FNS_APS_data_2020.RDS")))

FNS_APS_data <- readRDS("/Users/mariosaraiva/Documents/WB/Health/data/FNS_APS_data.RDS")

FNS_APS_data_rest <- readRDS("/Users/mariosaraiva/Documents/WB/Health/data/FNS_APS_data_rest.RDS")
                        
                        
FNS_APS_data_final <- rbind(FNS_APS_data, FNS_APS_data_rest)

saveRDS(FNS_APS_data_final, paste0(here("Documents", "WB","Health","data/","FNS_APS_Data.RDS")))
