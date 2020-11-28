#SISAB Parsing and Merging


library(readr)
library(dplyr)
library(here)
library(stringi)

read_plus <- function(flnm) {
  fread(flnm) %>% 
    mutate(filename = flnm)
}


SISAB_2019_2020 <- 
  list.files(path = here("Previne","data", "raw", "SISAB", "Producao"), 
             pattern = ".csv",
             recursive = T,
             full.names = T) %>%
  map_df(~read_plus(.))

SISAB_2019_2020 <- data.frame()

for (i in 1:19) {
  
  csv <- read_delim(here("data", "sisab", paste0(myfiles[i])), 
                    ";", escape_double = FALSE, trim_ws = TRUE)

  csv$file <-  paste0(myfiles[i])
  csv$mes <- stri_extract(myfiles[i], regex = "_\\s*(.*?)\\s*_")
  csv$ano <- stri_extract(myfiles[i], regex = "_20\\s*(.*?)\\.csv")
  csv$mes <- gsub("[_]", "", csv$mes)
  csv$ano <- gsub("[_]", "", csv$ano)
  csv$ano <- gsub(".csv", "", csv$ano)
  
  SISAB_2019_2020 <- rbind(SISAB_2019_2020, csv)

}


saveRDS(SISAB_2019_2020, paste0(here("data/", "SISAB_2019_2020.RDS")))
        
        