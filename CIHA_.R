#Comunicacao de internação Hospitalar e Ambulatorial - CIHA
#http://www2.datasus.gov.br/DATASUS/index.php?area=0901&item=1&acao=24&pad=31655



library(here)
library(tictoc)
library(dplyr)
library(read.dbc)

#install.packages("read.dbc")
#set_here("Previne")

#sus <- readdbc("RDTO1901dbc")

#1. List data files ####
myfiles <- list.files(path = here("Previne/data/raw/CIHA"), "dbc$")

#3. Process data####
##Filter database to extract only potentially avoidable situations
SIH_AIH_red_2019 <- data.frame()
total_obs_2019 <- data.frame()

length(myfiles)
for (i in 1:1) {
  
  df_dbc <- read.dbc(here(paste0("Previne/data/raw/CIHA/", myfiles[1])))
  
  df_dbc <- df_dbc[, tokeep]
  
  ##Extract total number of obs by month
  obs <- df_dbc %>%
    group_by(UF_ZI, ANO_CMPT ,MES_CMPT, COMPLEX) %>%
    summarise(total = n()) 
  obs <- as.data.frame(obs)
  
  total_obs_2019 <- rbind(total_obs_2019, obs)
  
  ##Extract data
  df_dbc$file <- paste0(myfiles[i])
  
  df_dbc_ACSC <- df_dbc %>%
    filter(DIAG_PRINC %in% ACSC_BR) #Filter by ACSC CID10 conditions.
  
  SIH_AIH_red_2019 <- rbind(SIH_AIH_red_2019, df_dbc_ACSC)
  
  if (i == 50) {print(i/length(myfiles)*100)}
  if (i == 100) {print(i/length(myfiles)*100)}
  if (i == 150) {print(i/length(myfiles)*100)}
  if (i == 200) {print(i/length(myfiles)*100)}
}
