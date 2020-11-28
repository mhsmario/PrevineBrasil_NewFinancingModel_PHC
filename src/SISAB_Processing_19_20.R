#SISAB data processing


library(dplyr)
library(here)
library(tidyr)
library(stringi)

setwd(here("Previne"))

mydatafiles <- list.files(path = here("Previne/data/rds"))

SISAB_2019_2020 <- readRDS(here("Previne/data/rds/SISAB_2019_2020.RDS"))

###Fixing Encoding, Month and Year####
sis_names <- names(SISAB_2019_2020)
Encoding(sis_names) <- "latin1"
Encoding(SISAB_2019_2020$Municipio) <- "latin1"
colnames(SISAB_2019_2020) <- sis_names

SISAB_2019_2020$ano <- gsub("2020", "202", SISAB_2019_2020$ano)
SISAB_2019_2020$ano <- gsub("202", "2020", SISAB_2019_2020$ano)

rm_accent <- function(str,pattern="all") {
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str) 
  
  return(str)
}

SISAB_2019_2020$Municipio <-  rm_accent(SISAB_2019_2020$Municipio)

SISAB_2020 <- SISAB_2019_2020 %>%
  filter(ano == 2020)

SISAB_2019 <- SISAB_2019_2020 %>%
  filter(ano == 2019)

saveRDS(SISAB_2020, here("Previne/data/rds/SISAB_2020.RDS")) 
saveRDS(SISAB_2019, here("Previne/data/rds/SISAB_2019.RDS")) 


