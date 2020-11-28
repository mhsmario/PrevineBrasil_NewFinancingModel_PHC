library(dplyr)

AIH_2019 <- readRDS("C:/Users/wb441213/OneDrive - WBG/Documents/Previne/data/rds/dataSIH_ACSC_AIH_red_2019.RDS")

AIH_2018 <- readRDS("C:/Users/wb441213/OneDrive - WBG/Documents/Previne/data/rds/dataSIH_ACSC_AIH_red_2018.RDS")

AIH_total_2018 <- readRDS("C:/Users/wb441213/OneDrive - WBG/Documents/Previne/data/rds/total_obs_2018_b.RDS")

AIH_total_2019 <- readRDS("C:/Users/wb441213/OneDrive - WBG/Documents/Previne/data/rds/total_obs_2019_b.RDS")


AIH_total_2019_short <- AIH_2019 %>%
  group_by(UF_ZI, ANO_CMPT, MES_CMPT, CEP, MUNIC_RES) %>%
  summarise(vltotal = sum(VAL_TOT), num.AIH = n())
  

AIH_result_2018 <- nrow(AIH_2018)/AIH_total_2018*100

AIH_result_2019 <- nrow(AIH_2019)/AIH_total_2019*100
#saveRDS(total_2018, file = "C:/Users/wb441213/OneDrive - WBG/Documents/Previne/data/total_obs_2018_b.RDS")
