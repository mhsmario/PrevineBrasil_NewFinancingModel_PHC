library(ggvis)
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

# For dropdown menu
actionLink <- function(inputId, ...) {
  tags$a(href = 'javascript:void',
         id = inputId,
         class = 'action-button',
         ...)
}

dashboardPage(
  dashboardHeader(title = "Previne results: First glance"),
  
  
  # titlePanel("Previne Analysis"),
  
  #Sidebar####
  dashboardSidebar(
   
             h4("Filtros"),
             
           
             
             sliderInput("popsize", "1. População do Município (Pop. size)",
                         784, 12252023,
                         value = c(784, 12252023), sep = ",", step = 100),
             
             selectInput("xvar", "2. Eixo-X (x-axis)", axis_vars, selected = "UF"),
             selectInput("yvar", "3. Eixo-Y (y-axis)", axis_vars_numeric, selected = "dif_relative"),
           
             
             selectInput("region", "4. Região (Region)",
                       c("All", "Centro-oeste", "Norte", "Nordeste",
                         "Sul", "Sudeste") ), 
             textInput("estado", 
                       "5. Nome do Estado (name of state) (e.g., `Toc`)"),
             textInput("municipio", 
                       "6. Nome do Município (name of Mun.)  (e.g., `Paulo`)"),
             
             selectInput("group", "7.Grupo (group)",
                         c("All", "Grande Acrescimo", "Acrescimo", "Decrescimo",
                           "Grande Decrescimo") ),

             selectInput("tipologia", "8. Tipologia (type)",
                         c("All", "IntermediarioAdjacente", 
                           "IntermediarioRemoto","RuralAdjacente", 
                           "RuralRemoto","Urbano")),
             
             selectInput("porte", "9. Porte (size)",
                         c("All", "P.Ate_20mil", "P.De20_49mil", "P.De50_100mil",
                           "P.De100_300mil", "P.Mais_de_300mil") ),
             
             selectInput("capital", "10. Capital (Main city)",
                       c("All", "Yes", "No") ), 
             
             selectInput("group_transicao", "11. Resultados com/sem repasses de transição?", c("Com Transição", "Sem Transição") ),
             
             selectInput("fns_65584", "12. Recebe Per Capita de Transição (65584)?",
                       c("All", "Yes", "No") ),
             selectInput("fns_65586", "13. Recebe Fator Comp. de Transição (65586)?",
                         c("All", "Yes", "No") ),
           
             tags$small(paste0(
               "Nota: Esse app foi construido para facilitar a exploração dos dados do Previne Brasil. Para melhor compreender as variaveis usadas consulte a aba 'Dicionário'.")),
             tags$small(paste0(
               "Note: This app is meant for exploration only. Please refer to the dataset's dictionary to better understand each variable displayed."))
             #)
    ),#sidebar
  
  ##Body####
    dashboardBody(
        tabsetPanel(
    ###TAB 1 - Overview####
                  tabPanel("Overview",
                       h4("Acrescimo and Decrescimo"),
                       HTML("<em> Use the x-axis and y-axis 
                                  filters to customize the graph. 
                            <br> Tip: Use the mouse to see the 
                            Municipality details. </em>"),
                       
                       wellPanel(
                                  HTML("<h4>Sobre o Previne Brasil </h4>
                                <br> A <a href = 'https://www.in.gov.br/en/web/dou/-/portaria-n-2.979-de-12-de-novembro-de-2019-227652180'>portaria nº 2.979, de 12 de novembro de 2019</a>, instituiu o <a href='https://aps.saude.gov.br/gestor/financiamento'>Programa Previne Brasil (PB)</a>, que estabelece novo modelo de financiamento de custeio da Atenção Primária à Saúde no âmbito do Sistema Único de Saúde (SUS). De acordo com o Ministério da Saúde, <i>'[a] proposta tem como princípio a estruturação de um modelo de financiamento focado em aumentar o acesso das pessoas aos serviços da Atenção Primária e o vínculo entre população e equipe, com base em mecanismos que induzem à responsabilização dos gestores e dos profissionais pelas pessoas que assistem'</i>. O novo modelo de transferências federais busca incentivar o aumento da cobertura (cadastro) da Atenção Primária à Saúde (APS), principalmente entre as populações vulneráveis, assim como, melhorar os resultados de saúde da população (desempenho da APS). Logo o Previne Brasil simplifica as transferências da APS em um modelo misto de financiamento: (I) Capitação ponderada, (II) Pagamento por desempenho , e (III) incentivos a programas específicos/estratégicos.
                                       <br> Apesar de haver simulações sobre o possível impacto do Previne Brasil, ainda não se sabe de fato os resultados alcançados pelo novo modelo de financiamento em seu primeiro ano de implementação. Importantes questionamentos surgem com o intuito de assegurar que o Previne Brasil está logrando os resultados almejados. 
                                       <h5>Esse painel explorativo dispõe de graficos para ajudarem a melhor entender os resultados preliminares do PB.</h5>"),
                                  HTML("<br> <br> <b>Número de Municípios selecionados:</b>"),
                                  textOutput("n_mun") %>% 
                                    withSpinner(color = "#0dc5c1"),
                                  textOutput("groups_txt"),
                                  textOutput("GA_txt"),
                                  textOutput("A_txt"),
                                  textOutput("D_txt"),
                                  textOutput("GD_txt")
                                        ), #wellPanel 
                       column(12,
                       div(style = "width:600px;",        
                                    
                           HTML("<em> Graph 1: Municipalities by groups </em>"),
                                    ggvisOutput("plot1"),
                           HTML("Filtros selecionados: "),
                           textOutput("displayfilter")))
 
                             ), #Tab 1
    
    ###TAB 2 - Grupos: Acréscimo X Decréscimo####
                  tabPanel("Grupos: Acréscimo X Decréscimo",
                           h4("Observing differences between groups"),
                           HTML("<em> Mean displayed at the center. 
                                <br>Tip: Use the mouse to see summary statistics, 
                                click and drag to zoom in. </em>"),
                           HTML("<br> <em> Graphs 2 and 3: Municipalities 
                                by Acrescimo and Grande Acrescimo </em>"),
                           splitLayout(plotlyOutput("Acrescimo") %>% 
                                         withSpinner(color = "#0dc5c1"), 
                            plotlyOutput("GrandeAcrescimo") %>% 
                                       withSpinner(color = "#0dc5c1")),
                           HTML("<em> Graphs 4 and 5: Municipalities 
                                by Decrescimo and Grande Decrescimo </em>"),
                           splitLayout(plotlyOutput("Decrescimo") %>% 
                                         withSpinner(color = "#0dc5c1"),
                                        plotlyOutput("GrandeDecrescimo") %>% 
                                         withSpinner(color = "#0dc5c1") )
                          ), #Tab 2
    
    ###TAB 3-  Proportions####
                  tabPanel("Proportions by Region and Porte",
                            h4("Graph 6: Differences by Region"),
                           HTML("<em> Pre-defined graphs to show proportions. 
                                <b>Y-Axis selection is disabled.</b> </em>"),
                          plotlyOutput("Prev_prop") %>% 
                                         withSpinner(color = "#0dc5c1"),
                          
                          HTML("<em> Graph 7: Registrations (red) and
                                Declared eSF Coverage (blue)</em>"),
                          plotlyOutput("Cad_Cob") %>% 
                                         withSpinner(color = "#0dc5c1")
                           
                           
                           ),#Tab 3
    
    #TAB 4 - Repasses####  
    tabPanel("Repasses",
             h4("Evolução dos Repasses segundo as categorias do Previne"),
             
             HTML("<em> Por enquanto só é possivel calcular a evolução da Capitação Ponderada.</b> </em>"),
             plotlyOutput("EvolucaoRepasses") %>% 
               withSpinner(color = "#0dc5c1")
             
    ), #TAB 4
    
    
    #TAB 5 - Dictionary####  
                  tabPanel("Variable Dictionary",
                           h4("Description of variables"),
                           HTML("<em> Use the search bar to find the details
                                on the variable of interest.</b> </em>"),
                           DTOutput("dict") %>% 
                             withSpinner(color = "#0dc5c1")
             
    ), #TAB 5
                       
    #TAB 6 - Contato####  
    tabPanel("Contato",
             h3("Contato "),
             HTML("Mario Saraiva - s.mario@columbia.edu"),
             HTML("<h4>Disclaimer: The views expressed in this app are the 
                  responsibility of the author.</h4>")
             
             ) #TAB 6
    ),#TabPannels
    tags$head(
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #7796AF}")),
      tags$style("#group ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" ),
      
      tags$style("#xvar ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" ),
      
      tags$style("#yvar ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" ),
      
      tags$style("#region ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" ),
      
      tags$style("#Tipologia ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" ),
      
      tags$style(" #capital ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" ),
      
      tags$style(" #fns_65584 ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" ),
      
      tags$style(" #fns_65586 ~ .selectize-control .option:nth-child(odd) {
                 background-color: #CACCCD;" )
      ) #HeadTags
    
    ) #body
  
  ) # page

