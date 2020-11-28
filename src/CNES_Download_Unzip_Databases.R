#CNES Databases


#Libraries####
library(dplyr)
library(here)
library(tidyr)
library(stringi)
library(tictoc)

setwd(here("Previne"))

##1. Download todas as bases de 2020####

#ftp://ftp.datasus.gov.br/cnes/BASE_DE_DADOS_CNES_202007.ZIP

###1.1 Crie os links de cada mes (base) para download####
url.root <- "ftp://ftp.datasus.gov.br/cnes/BASE_DE_DADOS_CNES_"
pasta_destino <-  paste0(here("Previne","data", "raw","CNES_bases"))

#ano <- "2020"
ano <- "2019"
#meses_20 <- "01"
#meses_20 <- c("01", "02", "03", "04", "05", "06", "07", "08")
#meses_20 <- c("02", "03", "04", "05", "06", "07", "08")
meses_2019 <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

CNES_urls <- character()
BD_file_name_root <- "BASE_DE_DADOS_CNES_"
CNES_DB <-  character()

for (i in meses_2019) {
  
  #Create urls
  url_loop <- paste0(url.root, ano, i, ".ZIP") 
  CNES_urls <- rbind(CNES_urls, url_loop)
 
  #Create file names for unzip in step 1.2
  db_file_name <- paste0(BD_file_name_root, ano, i, ".ZIP")
  CNES_DB <- rbind(CNES_DB, db_file_name)
}

###1.2 Download e unzip de cada base####

CNES_urls <- CNES_urls[-1,]
CNES_DB <- CNES_DB[-1,]

tic("Loop starts")
k = 0
for (i in CNES_urls) { 
 
  k = k + 1
  
  tic(paste0("Iteration = ", k))
  #Download database
  download.file(url = paste0(i), destfile = paste0(pasta_destino, CNES_DB[k]), mode = "wb")
  
  #Unzip files
  zipF <- paste0(pasta_destino, CNES_DB[k])
  outDir <- paste0(pasta_destino,BD_file_name_root, ano, k)
  unzip(zipF, exdir = outDir)
  
  #Move to the next month and file name
  
  if (k > 12) {
    print("Ops, something went wrong there's no month 13...")
    break
  }
  
  print(toc())
}
toc()

###1.3 Move and delete files ####

####1.3.1 Move relevant files ####

#A base do CNES contem 107 tabelas. Precisamos mover somente as relevantes para
#o projeto.
relevent_CNES_tables <- c("rlEstabComplementar", "tbEstabelecimento", "tbMantenedora",
                          "rlEstabAtendPrestConv", "tbDadosProfissionalSus", 
                          "tbEquipeChDifer", "tbEquipe", "rlEstabEquipeProf",
                          "tbEquipeAtendCompl", "rlEquipeNasfEsf",
                          "tbEstabHorarioAtend", "tbLeito", "tbFluxoDadosClientela",
                          "tbAtendimentoPrestado", "tbMunicipio", "tbTipoUnidade",
                          "tbTurnoAtendimento", "tbGestao", "tbAtividadeProfissional",
                          "tbAtributo", "tbTipoEquipe", "tbTipoCgDifer", 
                          "tbNaturezaJuridica", "tbGrupoEquipe", "tbTipoEqSubTipo",
                          "tbSubTipoEquipe", "tbGrupoAtividade", "tbAtividade",
                          "tbTipoEstabelecimento", "rlTipoEstabAtividade", "rlEstabProgFundo")

# file:///C:/Users/wb441213/OneDrive - WBG/Documents/Previne/data/raw/CNES_bases/2019/CNES_basesBASE_DE_DADOS_CNES_201901

tic()
BD_file_name_root <- "CNES_basesBASE_DE_DADOS_CNES_2019"
for (j in relevent_CNES_tables) { 
  
  for (k in meses_2019) { 
    
  thisfile <- paste0(j, ano, k, ".csv")
  
  thisfolder <- paste0(BD_file_name_root,k)
    
  from <- paste0(here("Previne","data", "raw","CNES_bases",ano, thisfolder, thisfile))
  to <- paste0(here("Previne","data", "raw", "CNES_bases",ano, thisfile))
  
  
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive = TRUE)
  file.rename(from = from,  to = to)
  unlink(from, recursive = TRUE)
  }
  
}

toc()  


#1.3.2 Delete unwanted folder


unlink(paste0(here("data", "raw", thisfolder)), recursive = TRUE)


