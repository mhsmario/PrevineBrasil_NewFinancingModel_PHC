##FNS 2019

library(data.table)
library(stringr)
library(dplyr)

mytxtfiles <- list.files(path = "~/Documents/WB/Health/data",
           pattern = ".txt$",
           full.names = T)


txt_results <- data.frame()
for (i in 1:length(mytxtfiles)) {
  txt <- read.csv(paste0(mytxtfiles[i]), sep = ";", comment.char = "#", 
  stringsAsFactors = FALSE, header = FALSE)
  colnames(txt)[1] <- "content"
  txt_results <- rbind(txt_results, txt)
}

missing_jul_2020 <- "https://consultafns.saude.gov.br/recursos/repasse-dia/detalhar-pagamento?codigo=65578&count=10&dsCompetencia=MAR%2520de%25202020&nuAno=2020&nuMes=7&page=1&tipo=PROGRAMA"

txt_results <- rbind(txt_results,missing_jul_2020)

txt_results$content <- gsub("url: ", "", txt_results$content)
txt_results$content <- gsub(",", "", txt_results$content)

txt_results <- as.data.frame(unique(txt_results$content))
colnames(txt_results)[1] <- "content"

txt_results$txt_results <- as.character(txt_results$content)

txt_results2 <- dplyr::filter(txt_results, !grepl("google", content))

colnames(txt_results2)[1] <- "url"

txt_results2 <- dplyr::filter(txt_results2, !grepl("app", url))
txt_results2 <- dplyr::filter(txt_results2, !grepl("ufs", url))
txt_results2 <- dplyr::filter(txt_results2, !grepl("anos", url))

txt_results2$url <- trimws(txt_results2$url)

txt_results2 <- as.data.frame(unique(txt_results2$url))
colnames(txt_results2)[1] <- "url"

##Prepare table
txt_results2$codigo <- sub(".*codigo=", "", txt_results2$url)
txt_results2$codigo <- sub("&.*", "", txt_results2$codigo)

txt_results2$Competencia_Mes <- sub(".*dsCompetencia=", "", txt_results2$url)
txt_results2$Competencia_Mes <- sub("%.*", "", txt_results2$Competencia_Mes)

txt_results2$Competencia_Ano <- sub("&nuAno.*", "", txt_results2$url)
txt_results2$Competencia_Ano <- str_sub(txt_results2$Competencia_Ano, start = -4)

txt_results2$Ano_posted <- sub("&nuMes.*", "", txt_results2$url)
txt_results2$Ano_posted <- str_sub(txt_results2$Ano_posted, start = -4)

txt_results2$Mes_posted <- sub("&page.*", "", txt_results2$url)
txt_results2$Mes_posted <- str_sub(txt_results2$Mes_posted, start = -2)
txt_results2$Mes_posted <- gsub("=", "", txt_results2$Mes_posted)

txt_results2$Competencia_Mes <- ifelse(is.na(txt_results2$Competencia_Mes), paste0(txt_results2$Mes_posted), txt_results2$Competencia_Mes)

txt_results2$Programa_ou_Emenda <- sub(".*tipo=", "", txt_results2$url)
txt_results2$Programa_ou_Emenda <- gsub("%2520", " ",txt_results2$Programa_ou_Emenda)

txt_results2$Competencia_Mes <- gsub("ABR", "01", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("AGO", "08", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("DEZ", "12", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("FEV", "02", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("JAN", "01", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("JUL", "07", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("JUN", "06", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("MAI", "05", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("MAR", "03", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("NOV", "11", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("OUT", "10", txt_results2$Competencia_Mes)
txt_results2$Competencia_Mes <- gsub("SET", "09", txt_results2$Competencia_Mes)

saveRDS(txt_results2, "~/Documents/WB/Health/data/FNS_URL_20_19_18.RDS")
write.csv(txt_results2, "~/Documents/WB/Health/data/FNS_URL_20_19_18.csv")

check <-  txt_results2 %>% 
  group_by(Ano_posted, Mes_posted) %>%
  filter(Ano_posted == 2018) %>% 
  summarise(n = n())
check$Mes_posted <- as.numeric(check$Mes_posted)
