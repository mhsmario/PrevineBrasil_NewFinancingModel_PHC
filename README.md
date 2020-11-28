# Previne Brasil New Financing Model of Brazil's Primary Healthcare
This repo contains the main scripts used to build and analyze Brazil's the new primary health care (PHC) financing model - Previne Brasil.
This readme file is a simple roadmap of the scripts used to build the Previne dataset using mostly public available data (sources below). The only private data source used refers to the number of registrations Q3/2019 (cadastros) by municipality received from the government.

One output of this project is a shiny dashboard that allow users to browser the Previne dataset.

##How the dataset was built?

The dataset was constructed by crossing different Health related databases,
specially the National Health Fund data on monthly PHC transfers to municipalities. 
The datasets and their sources are:

* Fundo Nacional de Saúde - [Repasses por dia/Custeio/Atencao Basica](https://consultafns.saude.gov.br/#/repasse-dia)

* SCNES - [Sistema de Cadastro Nacional de Estabelecimentos de Saúde Monthly database](http://cnes.datasus.gov.br/pages/downloads/arquivosBaseDados.jsp)

* SIH - [Sistema de Informações Hospitalares](http://www2.datasus.gov.br/DATASUS/index.php?area=0901)

* SISAB - Sistema de Informações da Atenção Básica 
    + [*Produçao* (número de atendimentos)](https://sisab.saude.gov.br/paginas/acessoRestrito/relatorio/federal/saude/RelSauProducao.xhtml)
    + [*Cobertura*](https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/relHistoricoCoberturaAB.xhtml)
    + [*Cadastros Individuais*](https://sisaps.saude.gov.br/painelsaps/cadastropop_pub)
    + [*Painel de Cadastros* ](https://sisab.saude.gov.br/paginas/acessoRestrito/relatorio/federal/indicadores/indicadorCadastro.xhtml)

* IBGE - [GDP per Municipality (2017),Census and PNADC ](https://www.ibge.gov.br/estatisticas/downloads-estatisticas.html)

* COVID19 - [COVID19 Dashboard](https://covid.saude.gov.br/)


##Scripts and outputs used

### Fundo Nacional de Saúde

**Important remarks**
The FNS data was scrapped partially using a python script - `FNS_Webscrapping.ipynb` to click all the buttons and partially by manually selecting the links of each dataset by motoring the browser's Network developer tab when each link was clicked. Thus a dataset was built with FNS urls only for 2018, 2019 and 2020. Rstudio was used to systematically scrape all URL's Json files - `FNS_ParseURLs`, `FNS_Json_parsing.R`. This section was done outside the banks computer due to security restrictions or unupdated version of R that prevent using certain packages. 

>It is important to note that the FNS data comes with two different dates: (1) the date that the transcation was posted and (2) the compensaiton date. For example, municipality X achieved certain targets thus one (or x) months later is entitled to receive a transfer from FNS. **The later was selected as it reflects the compensation received relative to performance/target achievements.**

The data scrapped was exported in 3 files - `FNS_APS_data_2018.RDS`, `FNS_APS_data_2019.RDS`, `FNS_APS_data_2020.RDS` - according to the year of which the transactions were posted. For example, all transactions that appeared on the website when filtered by the desired year (instead of the year of reference for the transfer). That was the only part of the analysis done outside. 

`"FNS_Processing.R"` is the main script used to processed (wrangle) FNS data such as fixing and standardizing the Mes variable.

* Input files: `FNS_APS_data_2018.RDS`, `FNS_APS_data_2019.RDS`, `FNS_APS_data_2020.RDS`

* Output files: `FNS_data_2018_long.RDS`, `FNS_data_2018_wide.RDS`, `FNS_data_2019_long.RDS`, `FNS_data_2019_wide.RDS`, `FNS_data_2020_long.RDS`, `FNS_data_2020_wide.RDS`.

####Other FNS related scripts
FNS_Comparing FNS FAF e Repasses detalhados.R - Used to compare another FNS dataset (Transferencias Fundoa a Fundo). However, such dataset limits the data to direct transfers to other local government funds. The dataset used for this project from the FNS website under Repasses por dia is more rich in data.

###IBGE
Regarding the Municipalities GDP, no scripts used and the data was slitghly wrangle in the Previne_Var_ scripts. However, the Poverty lines were produced using Stata and Excel using the Census and PNADC.

###SCNES
The SCNES database is complex and relatively large (about 5GBs+ each containing approximately 107 csv files). The database is systematically presented in the **SCNES_DICIONARIO_DE_DADOS_PREVINE_PROJECT.docx** dictionary (available at http://cnes.datasus.gov.br/pages/downloads/documentacao.jsp)

The script - `CNES_Download_Unzip_Databases.R` downloads and unzips the monthly files per year. To clean and assemble the final. There is not an input file for this script as it constructs the urls of each download based on the year desired by the user. The output files for the script are the several csv tables.   

The scripts `CNES_Variables_2020.R` and `CNES_Variables_2019.R` cleans, merges and wrangles the csv files needed to create the desired variables.

The scripts `CNES_Variables_part2_2020.R` and `CNES_Variables_part2_2019.R` puts together all the variables created in the previous scripts into one single dataset. 

>2020

* Input files: `CNES_ESF_equipes_2020.RDS`, `CNES_ESF_Horario2020.RDS`, `CNES_Profissionais_APS.RDS`, `Municipios.RDS`

* Output file: `CNES_Complete_2020.RDS`

>2019

* Input files: `CNES_ESF_equipes_2019.RDS`, `CNES_ESF_Horario2019.RDS`, `CNES_Profissionais_APS.RDS`, `Municipios.RDS`
(CHECK IF PROFISSIONAIS_APS is correctly generated)

* Output file: `CNES_Complete_2019.RDS`


###SISAB

`SISAB_Processing_19_20.R`
`SISAB_Cobertura.R`
`SISAB_FNS_plots.R`
`SISAB_joiningfiles.R`

Cobertura: O SISAB, na [nota metodologica](https://egestorab.saude.gov.br/paginas/acessoPublico/relatorios/relHistoricoCoberturaAB.xhtml) dispoe sobre o calculo usado para estimar a cobertura tanto da Atenção Básica como das equipes de Saude Familiar (eSF). Método de cálculo: 

$$Cobertura \ AB = \frac{nº \ eSF * 3450 + (nº \ eAB \ param. + nº \ eSF \ equivalentes * 3000)*100} {Estimativa \ Populacional}$$ 
No qual: 

*nº eSF*: número de equipes de Saúde da Família com códigos 1 a 3, 12 a 15,
24 a 39, desde que vinculadas aos estabelecimentos de saúde instituídas
em sua respectiva portaria e cadastradas no SCNES. As equipes de 24 a
38 serão ponderadas conforme Portaria nº 703/2011 (24 a 26 = 1 equipe;
27 a 29 = 2 equipes; 30 a 32 = 3 equipes; 33 a 35 = 0,85 equipe; 36 a 38
= 0,6 equipe). Inclusão:  foram consideradas equipes de Saúde da Família com código 70 de acordo com as regras estabelecidas na Portaria nº 99, de 7 de fevereiro de 2020. No método de cálculo, a equipe com código 70 é classificada como eSF com ponderação = 1 equipe. Caso as eSF (1 a 3, 12 a 15, 24 a 39 e 70) não cumpram os critérios (quantidade mínima de profissionais, CBO e carga horária semanal) e tenham o registro mais recente no SCNES, entre janeiro e abril de 2020, como EAB (16 a 21), serão consideradas como EAB para o cálculo de cobertura.

*nº eAB param.*: número de equipes de Atenção Básica parametrizadas com
códigos de 16 a 21, desde que vinculadas aos estabelecimentos de saúde
instituídas em sua respectiva portaria e cadastradas no SCNES. As
equipes serão ponderadas conforme Portaria nº 576/2011 (16 e 19 = 1
equipe; 17 e 20 = 2 equipes; 18 e 21 = 3 equipes). Inclusão: foram consideradas equipes de Atenção Primária (eAP) com código 76 de acordo com as regras estabelecidas na Portaria nº 99, de 7 de fevereiro de 2020. No método de cálculo, a equipe com código 76 é classificada como EAB parametrizada com ponderação = 1 equipe.

*nºeSF equivalentes*: o mínimo de 60h de carga horária ambulatorial médica
e mínimo de 40h de carga horária ambulatorial de enfermagem na
Atenção Básica equivale a uma equipe. Considera-se sempre o menor
valor entre os quocientes, desde que o resultado seja no mínimo 1.

*Parâmetro*: considera o valor de 3.450 indivíduos cobertos por equipe de
Saúde da Família, e 3.000 indivíduos cobertos pelas equipes de atenção
básica parametrizadas e equipes equivalentes, resultados da média
aritmética entre os valores mínimo e máximo definidos na PNAB 2011.

*Estimativa populacional*: será considerada sempre a estimativa do ano
anterior, e atualizada no mês de janeiro, para fins de cálculo do indicador.


###SIH

`SIH_2018.R`
`SIH_2019.R`
`SIH_2020.R`


###COVID19
`COVID19.R`

## Previne Dataset
The scripts - `1.Previne_Var_2020.R`,`2.Previne_Var_2019.R`,`3.Previne_Var_2018.R` (to come)  generates the final 2020, 2019, and 2018 dataset for this project (according with the compensation date of each transfer). It combines all datasets generated in the previous sections into one file output.

>2020

* Input files: `FNS_data_2020_wide.RDS`, `IBGE_PIB_Municipios_2017.csv`, `CNES_Complete_2020.RDS`, `SISAB_2020.RDS`, `SIH_AIH_ACSC_2020_totals.RDS`, `COVID_Aug31.RDS`
* Output files: `Previne_dataset_2020.RDS`, `Previne_dataset_2020.dta`

>2019

* Input files: `FNS_data_2019_wide.RDS`, `IBGE_PIB_Municipios_2017.csv`, `CNES_Complete_2019.RDS`, `SISAB_2019.RDS`, `SIH_AIH_ACSC_2019_totals.RDS`

* Output files: `Previne_dataset_2019.RDS`, `Previne_dataset_2019.dta`

>2018

* **TO COME**

###Previne dataset check
The script - `Previne_dataset_check.R` checks the quality and completness of the final 2020 dataset and the other datasets.

###Previne Join
The script - `4. Previne_Join_2020_2019.R` puts together the Previne 2019 and 2020 datasets, performs additional adjustments to the data, and exports the final dataset as .RDA and .DTA files.


--------------
Author: *Mario Saraiva, STC, World Bank/Brazil* - msaraiva@worldbank.org 
