
#0. Project Info####
##PROCESSAMENTO DOS DADOS DE INTERNAÇÕES HOSPITALARES
##ANO = 2018

#Site que fala um pouco sobre onde encontrar os dados do SUS.
#https://dadosabertos.social/t/como-obter-e-tratar-dados-do-datasus/66

#Datasus - Internações Hospitalares

#Download de microdados e documentação da base de dados - http://www2.datasus.gov.br/DATASUS/index.php?area=0901'

#Para entender a fundo os dados coletados pelo Sistema de Info Hospitalar

#'Download do MANUAL TÉCNICO DO SISTEMA DE INFORMAÇÃO HOSPITALAR - http://bvsms.saude.gov.br/bvs/publicacoes/07_0066_M.pdf'


library(here)
library(tictoc)
library(dplyr)
library(read.dbc)

#install.packages("read.dbc")
#set_here("Previne")

#sus <- readdbc("RDTO1901dbc")

#1. List data files ####
myfiles <- list.files(path = here("Previne/data/raw/2018"), "dbc$")

tic("total")

#2. Prep. dimension reduction parameters ####
tokeep <- c(1:10, 23:26, 36, 38, 39, 40:42, 44, 45, 49:52, 56, 58, 59, 61,62,65,66,75,78,80,84,85,86,96,97,98,105,107)

#Select COD-10 ACSC codes
ACSC_BR <- c("A00", "A00", "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A150", "A151", "A152", "A153", "A154", "A155", "A156", "A157", "A158", "A159", "A160", "A161", "A162", "A163", "A164", "A165", "A166", "A167", "A168", "A169", "A170", "A171", "A172", "A173", "A174", "A175", "A176", "A177", "A178", "A179", "A18", "A19", "A33", "A34", "A35", "A36", "A37", "A46", "A50", "A51", "A52", "A53", "A95", "B05", "B06", "B16", "B26", "B50", "B51", "B51", "B51", "B51", "B52", "B52", "B52", "B52", "B53", "B53", "B53", "B53", "B54", "B77", "D50", "E100", "E101", "E102", "E103", "E104", "E105", "E106", "E107", "E108", "E109", "E110", "E111", "E112", "E113", "E114", "E115", "E116", "E117", "E118", "E119", "E120", "E121", "E122", "E123", "E124", "E125", "E126", "E127", "E128", "E130", "E131", "E133", "E134", "E135", "E136", "E137", "E138", "E139", "E140", "E141", "E143", "E144", "E145", "E146", "E147", "E148", "E40", "E41", "E42", "E43", "E44", "E45", "E46", "E50", "E51", "E52", "E53", "E54", "E55", "E56", "E57", "E58", "E59", "E60", "E61", "E63", "E63", "E64", "E86", "G000", "G40", "G41", "G45", "G46", "H66", "I00", "I02", "I10", "I11", "I20", "I50", "I63", "I64", "I65", "I66", "I67", "I69", "J00", "J01", "J02", "J03", "J06", "J13", "J14", "J153", "J154", "J158", "J159", "J181", "J20", "J21", "J31", "J40", "J41", "J42", "J43", "J44", "J45", "J46", "J47", "J81", "K25", "K26", "K27", "K28", "K920", "K921", "K922", "L01", "L02", "L03", "L04", "L08", "N10", "N11", "N12", "N30", "N34", "N390", "N70", "N71", "N72", "N73", "N75", "N76", "O23", "P350")

tic("loop_ACSC")

#3. Process data####
##Filter database to extract only potentially avoidable situations
SIH_AIH_red_2018 <- data.frame()
total_obs_2018 <- data.frame()

for (i in 1:length(myfiles)) {
  
  df_dbc <- read.dbc(here(paste0("Previne/data/raw/2018/", myfiles[i])))
  
  df_dbc <- df_dbc[, tokeep]
  
  ##Extract total number of obs by month
  obs <- df_dbc %>%
    group_by(UF_ZI, ANO_CMPT ,MES_CMPT, COMPLEX) %>%
    summarise(total = n()) 
  obs <- as.data.frame(obs)
  
  total_obs_2018 <- rbind(total_obs_2018, obs)
  
  ##Extract data
  df_dbc$file <- paste0(myfiles[i])
  
  df_dbc_ACSC <- df_dbc %>%
    filter(DIAG_PRINC %in% ACSC_BR) #Filter by ACSC CID10 conditions.
  
  SIH_AIH_red_2018 <- rbind(SIH_AIH_red_2018, df_dbc_ACSC)
  
  if (i == 50) {print(i/length(myfiles)*100)}
  if (i == 100) {print(i/length(myfiles)*100)}
  if (i == 150) {print(i/length(myfiles)*100)}
  if (i == 200) {print(i/length(myfiles)*100)}
}


#ACSC_Result_2018 <- nrow(SIH_AIH_red_2018)/total_obs_2018_final


#4. Save data frame####
saveRDS(SIH_AIH_red_2018, file = here("Previne/data/SIH_ACSC_AIH_red_2018.RDS"))
saveRDS(total_obs_2018, file = here("Previne/data/total_obs_UF_MES_2018.RDS"))

toc()
toc()
