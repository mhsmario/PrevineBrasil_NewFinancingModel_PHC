#Libraries####
library(ggvis)
library(dplyr)
if (FALSE) {
  library(RSQLite)
  library(dbplyr)
}

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
library(reshape2)
library(DT)
library(readxl)


#Server####
function(input, output, session) {

#Tab 1#### 
  
##1. Reactive Dataset####    
  
  # Filter the Previne Annual dataset, returning a data frame
  Previne <- reactive({
    
    # Due to dplyr issue #318, we need temp variables for input values
    minpopsize <- input$popsize[1]
    maxpopsize <- input$popsize[2]
    # Apply filters
    m <- Previne_a_2 %>%
      filter(SISAB_C_POP.MUN >= minpopsize,
             SISAB_C_POP.MUN <= maxpopsize) %>%
      arrange(group)
 
     # Optional: filter by group

    if (input$group != "All") {
       Group <- paste0(input$group)
       m <- m %>% filter(group == Group)
     } 
       
    # Optional: filter by region
    if (input$region != "All") {
      Region <- paste0(input$region)
      m <- m %>% filter(IBGE_NO_G_Reg == Region)
    }
     # Optional: filter by Tipologia
     if (input$tipologia != "All") {
       Tipologia <- paste0(input$tipologia)
       m <- m %>% filter(Tipologia_C_50 == Tipologia)
     }
    # Optional: filter by Porte
    if (input$porte != "All") {
      Porte <- paste0(input$porte)
      m <- m %>% filter(Porte_Mun == Porte)
    }
    # Optional: filter by Capital
    if (input$capital != "All") {
      Capital <- paste0(input$capital)
      Capital_B <- ifelse(Capital == "Yes", 1,0)
      m <- m %>% filter(capital == Capital_B)
    }

    # Optional: filter by Recebe_PerCapita_de_Trans
    if (input$fns_65584 != "All") {
      fns_65584 <- paste0(input$fns_65584)
      fns_65584_B <- ifelse(fns_65584 == "Yes", 1,0)
      m <- m %>% filter(Recebe_PerCapita_de_Trans == fns_65584_B)
    }

    # Optional: filter by Recebe_Fator_Comp_de_Trans
    if (input$fns_65586 != "All") {
      fns_65586 <- paste0(input$fns_65586)
      fns_65586_B <- ifelse(fns_65586 == "Yes", 1,0)
      m <- m %>% filter(Recebe_Fator_Comp_de_Trans == fns_65586_B)
    }
    
     # Optional: filter by Municipio
     if (!is.null(input$municipio) && input$municipio != "") {
       Municipio <- toupper(paste0(input$municipio))
       m <- m %>% filter(NO_MUNICIPIO %like% Municipio)
     }
    # Optional: filter by Estado
    if (!is.null(input$estado) && input$estado != "") {
      Estado <- str_to_title(paste0(input$estado))
      m <- m %>% filter(IBGE_NO_UF %like% Estado)
    }
    
    m <- as.data.frame(m)
    
    
  })

##2. Scatter plot#### 
  #Function for generating tooltip text
  previne_tooltip <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.null(x$CO_MUNICIPIO)) return(NULL)
    
    yvar_name <- names(axis_vars_numeric)[axis_vars_numeric == input$yvar]
    yvas <- input$yvar
    
    if (input$group_transicao == "Sem Transição") { 
        if (yvar_name == "Taxa de mudança") {
          yvar_name <- "Taxa de mudança (Sem Transição)"
          yvas <- prop("y", as.symbol("dif_relative_sem_transicao"))
        }#Change dif_relative to dif_relative_sem transiçao
        
        if (yvar_name == "Diferença no repasse recebido (2020-2019)") {
          yvar_name <- "Diferença no repasse recebido (2020-2019) (Sem Transição)"
          yvas <- prop("y", as.symbol("dif_sem_transicao"))
        }#Change dif_percapita to dif_percapita_sem_transicao
        
        if (yvar_name == "Diferença no repasse recebido per capita") {
          yvar_name <- "Diferença no repasse recebido per capita (Sem Transição)"
          yvas <- prop("y", as.symbol("dif_percapita_sem_transicao"))
        }#Change dif_percapita to dif_percapita_sem_transicao
     
      Pr2 <- isolate(Previne())
      previne_2 <- Pr2[Pr2$CO_MUNICIPIO == x$CO_MUNICIPIO, ]
      
      paste0("<b>", previne_2$NO_MUNICIPIO, "-", previne_2$UF, "</b>",
             "<br><b>", previne_2$group_sem_transicao, "</b>",
             "<br>", yvar_name,": <b>", previne_2[,input$yvar], "</b><br>",
             "Porte: ",previne_2$Porte_Mun, "<br>",
             "Tipo: ",previne_2$Tipologia_C_50, "<br>",
             "Taxa de mudança (%):", previne_2$dif_relative, "<br>",
             "N. Cadastros: ", previne_2$Cadastro_total_Q3yr19, "<br>",
             "% Pop. cadastrada: ", previne_2$cadastro_total_pct)
    }# End If-statement
    
    else{
    # Pick out the Municipality with this ID
    Pr2 <- isolate(Previne())
    previne_2 <- Pr2[Pr2$CO_MUNICIPIO == x$CO_MUNICIPIO, ]
  
        paste0("<b>", previne_2$NO_MUNICIPIO, "-", previne_2$UF, "</b>
               <br><b>",previne_2$group, "</b><br>",
               yvar_name,": <b>", previne_2[,yvas], "</b><br>",
               "Porte: ",previne_2$Porte_Mun, "<br>",
               "Tipo: ",previne_2$Tipologia_C_50, "<br>",
               "Taxa de mudança (%):", previne_2$dif_relative, "<br>",
               "N. Cadastros: ", previne_2$Cadastro_total_Q3yr19, "<br>",
               "% Pop. cadastrada: ", previne_2$cadastro_total_pct)
      }#End of Else statement
  }

  #A reactive expression with the ggvis plot
  vis1 <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars_numeric)[axis_vars_numeric == input$yvar]
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    if (input$group_transicao == "Com Transição") { 
    Previne %>%
      
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 25, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   fill = ~group,
                   stroke = ~group, key := ~CO_MUNICIPIO) %>%
      add_tooltip(previne_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke",values = c("Grande Acrescimo", "Acrescimo", "Decrescimo", "Grande Decrescimo")) %>%
      scale_nominal("stroke", domain = c("Grande Acrescimo", "Acrescimo", "Decrescimo", "Grande Decrescimo")) %>%
       set_options(width = 1000, height = 500)
    }#End of If statement
    
    else { 
      
      Previne %>%
        
        ggvis(x = xvar, y = yvar) %>%
        layer_points(size := 25, size.hover := 200,
                     fillOpacity := 0.2, fillOpacity.hover := 0.5,
                     fill = ~group_sem_transicao,
                     stroke = ~group_sem_transicao, key := ~CO_MUNICIPIO) %>%
        add_tooltip(previne_tooltip, "hover") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        add_legend("stroke",values = c("Grande Acrescimo", "Acrescimo", "Decrescimo", "Grande Decrescimo")) %>%
        scale_nominal("stroke", domain = c("Grande Acrescimo", "Acrescimo", "Decrescimo", "Grande Decrescimo")) %>%
        set_options(width = 1000, height = 500)
    } #End of Else2 statement
  }) #end vis1

  vis1 %>% bind_shiny("plot1")
##3.0 Text display selected filters####

  output$displayfilter <- renderText({
    #paste0(input$xvar, input$yvar)
   selected_filters <- paste(" --> ","X = ", input$xvar, " | ",
                               "Y = ", input$Yvar, " | ",
                               "População: ", input$popsize, " - ", input$popsize[2], " | ",
                               "Região: ", input$region, " | ",
                               "Estado: ", input$estado, " | ",
                               "Município: ", input$municipio, " | ",
                               "Grupo: ", input$group, " | ",
                               "Tipologia: ", input$tipologia, " | ",
                               "Apenas capitais?: ", input$capital, " | ",
                               "Com/sem Transição? ", input$group_transicao
                               )

    })
##3.1 Text # of Elements#### 
  output$n_mun <- renderText({nrow(Previne())})
  output$GA_txt <- renderText({
    
    Prev <- Previne()
    if (input$group_transicao == "Com Transição") {
          GrandeAcrescimo <- nrow(Prev %>% filter(group == "Grande Acrescimo"))
    } else {
          GrandeAcrescimo <- nrow(Prev %>% filter(group_sem_transicao == "Grande Acrescimo"))
    }
    
    Gr <-  paste0("Grande Acréscimo: ", GrandeAcrescimo, " Municípios.")
    })
  output$A_txt <- renderText({
    
    Prev <- Previne()
    if (input$group_transicao == "Com Transição") {
          Acrescimo <- nrow(Prev %>% filter(group == "Acrescimo"))
      } else{
      Acrescimo <- nrow(Prev %>% filter(group_sem_transicao == "Acrescimo"))
    }
     
     Gr <-  paste0("Acréscimo: ", Acrescimo, " Municípios.")
  })
  
  output$D_txt <- renderText({
    Prev <- Previne()
    if (input$group_transicao == "Com Transição") {
      Decrescimo <- nrow(Prev %>% filter(group == "Decrescimo"))
    } else {
      Decrescimo <- nrow(Prev %>% filter(group_sem_transicao == "Decrescimo"))
    }
    
    Gr <-  paste0("Decréscimo: ", Decrescimo, " Municípios.")
    
  })
  
  output$GD_txt <- renderText({
    
    Prev <- Previne()
    if (input$group_transicao == "Com Transição") {
      GDecrescimo <- nrow(Prev %>% filter(group == "Grande Decrescimo"))
    } else {
      GDecrescimo <- nrow(Prev %>% filter(group_sem_transicao == "Grande Decrescimo"))
    }
    
    Gr <-  paste0("Grande Decréscimo: ", GDecrescimo, " Municípios.")
    
  })
#Tab 2#### 

##4. Output_Acrescimos####
output$Acrescimo <- renderPlotly({
  # Lables for axes
  xvar_name <- names(axis_vars)[axis_vars == input$xvar]
  yvar_name <- names(axis_vars_numeric)[axis_vars_numeric == input$yvar]
  
  inputy <- paste0(input$yvar)
  inputx <- paste0(input$xvar)
  
  
  if (input$group_transicao == "Sem Transição") { 
    if (yvar_name == "Taxa de mudança") {
      yvar_name <- "Taxa de mudança (Sem Transição)"
      inputy <- "dif_relative_sem_transicao"
    }#Change dif_relative to dif_relative_sem transiçao
    
    if (yvar_name == "Diferença no repasse recebido (2020-2019)") {
      yvar_name <- "Diferença no repasse recebido (2020-2019) (Sem Transição)"
      inputy <- "dif_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    if (yvar_name == "Diferença no repasse recebido per capita") {
      yvar_name <- "Diferença no repasse recebido per capita (Sem Transição)"
      inputy <- "dif_percapita_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    PR_win <- Previne() %>%
      filter(group_sem_transicao == "Acrescimo")
  } else { 
          PR_win <- Previne() %>%
          filter(group == "Acrescimo")
          }#End Else statement
 

   
  gplot <- ggplot(PR_win, aes_string(x = input$xvar, 
                                        y = inputy, fill = input$xvar)) +
    geom_boxplot() + 
    labs(y = yvar_name, x = xvar_name,title = "Teve Acréscimo", subtitle = "The mean displayed at the center. Hover to see summary statistics.") +
    stat_summary(fun = mean, colour = "darkred", geom = "point", 
                 size = .1, show.legend  =  FALSE) +
    stat_summary(fun = mean, colour = "grey19", geom = "text", show.legend  =  FALSE, 
                 vjust = -1, aes( label = round(..y.., digits = 2))) +
    theme(legend.position = "none")
   
  gg <- ggplotly(gplot) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot",
        width = 600,
        height = 700
      ) )
  gg
})

##5. Output - GrandeAcrescimos####
output$GrandeAcrescimo <- renderPlotly({
  # Lables for axes
  xvar_name <- names(axis_vars)[axis_vars == input$xvar]
  yvar_name <- names(axis_vars_numeric)[axis_vars_numeric == input$yvar]
  
  inputy <- paste0(input$yvar)
  inputx <- paste0(input$xvar)
  
  if (input$group_transicao == "Sem Transição") { 
    if (yvar_name == "Taxa de mudança") {
      yvar_name <- "Taxa de mudança (Sem Transição)"
      inputy <- "dif_relative_sem_transicao"
    }#Change dif_relative to dif_relative_sem transiçao
    
    if (yvar_name == "Diferença no repasse recebido (2020-2019)") {
      yvar_name <- "Diferença no repasse recebido (2020-2019) (Sem Transição)"
      inputy <- "dif_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    if (yvar_name == "Diferença no repasse recebido per capita") {
      yvar_name <- "Diferença no repasse recebido per capita (Sem Transição)"
      inputy <- "dif_percapita_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    PR_winbig <- Previne() %>%
      filter(group_sem_transicao == "Grande Acrescimo")
  } else { 
    PR_winbig <- Previne() %>%
      filter(group == "Grande Acrescimo")
  }#End Else statement

  
  gplot <- ggplot(PR_winbig, aes_string(x = input$xvar, 
                                     y = inputy, fill = input$xvar)) +
    geom_boxplot() + 
    labs(y = yvar_name, x = xvar_name, title = "Teve Grande Acréscimo") +
    stat_summary(fun = mean, colour = "darkred", geom = "point", 
                 size = .1, show.legend  =  FALSE) +
    stat_summary(fun = mean, colour = "grey19", geom = "text", show.legend  =  FALSE, 
                 vjust = -1, aes( label = round(..y.., digits = 2))) +
    theme(legend.position = "none")
  
  hh <- ggplotly(gplot) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot",
        width = 600,
        height = 700
      ) )
  hh
})

##6. Output - Decrescimos####
output$Decrescimo <- renderPlotly({
  # Lables for axes
  xvar_name <- names(axis_vars)[axis_vars == input$xvar]
  yvar_name <- names(axis_vars_numeric)[axis_vars_numeric == input$yvar]
  inputy <- paste0(input$yvar)
  inputx <- paste0(input$xvar)
  
  if (input$group_transicao == "Sem Transição") { 
    if (yvar_name == "Taxa de mudança") {
      yvar_name <- "Taxa de mudança (Sem Transição)"
      inputy <- "dif_relative_sem_transicao"
    }#Change dif_relative to dif_relative_sem transiçao
    
    if (yvar_name == "Diferença no repasse recebido (2020-2019)") {
      yvar_name <- "Diferença no repasse recebido (2020-2019) (Sem Transição)"
      inputy <- "dif_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    if (yvar_name == "Diferença no repasse recebido per capita") {
      yvar_name <- "Diferença no repasse recebido per capita (Sem Transição)"
      inputy <- "dif_percapita_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    PR_loose <- Previne() %>%
      filter(group_sem_transicao == "Decrescimo")
  } else { 
    PR_loose <- Previne() %>%
      filter(group == "Decrescimo")
  }#End Else statement


  gplot <- ggplot(PR_loose, aes_string(x = input$xvar, 
                                     y = inputy, fill = input$xvar)) +
    geom_boxplot() + 
    labs(y = yvar_name, x = xvar_name,title = "Teve Decréscimo") +
    stat_summary(fun = mean, colour = "darkred", geom = "point", 
                 size = .1, show.legend  =  FALSE) +
    stat_summary(fun = mean, colour = "grey19", geom = "text", show.legend  =  FALSE, 
                 vjust = -1, aes( label = round(..y.., digits = 2))) +
    theme(legend.position = "none")
  
  ii <- ggplotly(gplot) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot",
        width = 600,
        height = 700
      ) )
  ii
})

##7. Output - Grande Decrescimos####
output$GrandeDecrescimo <- renderPlotly({
  # Lables for axes
  xvar_name <- names(axis_vars)[axis_vars == input$xvar]
  yvar_name <- names(axis_vars_numeric)[axis_vars_numeric == input$yvar]
  inputy <- paste0(input$yvar)
  inputx <- paste0(input$xvar)
  
  if (input$group_transicao == "Sem Transição") { 
    if (yvar_name == "Taxa de mudança") {
      yvar_name <- "Taxa de mudança (Sem Transição)"
      inputy <- "dif_relative_sem_transicao"
    }#Change dif_relative to dif_relative_sem transiçao
    
    if (yvar_name == "Diferença no repasse recebido (2020-2019)") {
      yvar_name <- "Diferença no repasse recebido (2020-2019) (Sem Transição)"
      inputy <- "dif_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    if (yvar_name == "Diferença no repasse recebido per capita") {
      yvar_name <- "Diferença no repasse recebido per capita (Sem Transição)"
      inputy <- "dif_percapita_sem_transicao"
    }#Change dif_percapita to dif_percapita_sem_transicao
    
    PR_loosebig <- Previne() %>%
      filter(group_sem_transicao == "Decrescimo")
  } else { 
    PR_loosebig <- Previne() %>%
      filter(group == "Grande Decrescimo")
  }#End Else statement
  
  
  gplot <- ggplot(PR_loosebig, aes_string(x = input$xvar, 
                                     y = inputy, fill = input$xvar)) +
    geom_boxplot() + 
    labs(y = yvar_name, x = xvar_name,title = " Teve Grande Decréscimo") +
    stat_summary(fun = mean, colour = "darkred", geom = "point", 
                 size = .1, show.legend  =  FALSE) +
    stat_summary(fun = mean, colour = "grey19", geom = "text", show.legend  =  FALSE, 
                 vjust = -1, aes( label = round(..y.., digits = 2))) +
    theme(legend.position = "none")
  
  gg <- ggplotly(gplot) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot",
        width = 600,
        height = 700) )
  gg
})
   

#Tab3####
  
##8. Region Bar plot#### 

  output$Prev_prop <- renderPlotly({
    # Lables for axes

    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars_numeric)[axis_vars_numeric == input$yvar]

   
    inputx <- paste0(input$xvar)
   
      if (input$group_transicao == "Sem Transição") { 
        groupforplot <- paste0("group_sem_transicao")
      
        Previne_prop <- Previne() %>%
          group_by_(inputx, group_sem_transicao) %>%
        summarise(n = n()) %>%
        mutate(Freq = n / sum(n))
      
        reg <- ggplot(Previne_prop, 
                    aes_string(input$xvar, groupforplot,
                               fill = groupforplot)) + 
        geom_col() +
        #geom_text_repel(aes(label = scales::percent(Freq, accuracy = 2),
        #                 y = Freq)) +
        ylab(paste0("Proporção dos grupos por ", xvar_name)) #+
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none")
      } #end of IF statement
      else {
        groupforplot <- paste0("group")
        Previne_prop <- Previne() %>%
          group_by_(inputx, groupforplot) %>%
          summarise(n = n()) %>%
          mutate(Freq = n / sum(n))
        
        reg <- ggplot(Previne_prop, aes_string(inputx, 'Freq',
                                              fill = groupforplot)) + 
          geom_col() +
          #geom_text(aes(label = scales::percent(Freq, accuracy = 2),
          #                 y = Freq)) +
          ylab(paste0("Proporção dos grupos por ", xvar_name)) #+
        theme(#axis.text.y = element_blank(),
              #axis.ticks.y = element_blank(),
              legend.position = "none")
        
        }#end of Else statement
      
     
      
     jj <- ggplotly(reg) %>%
       config(
         toImageButtonOptions = list(
           format = "svg",
           filename = "myplot",
           width = 600,
           height = 700) )
     jj
  })#end vis2

##9. Cadastros e Cobertura Box plots#### 
   output$Cad_Cob <- renderPlotly({

    g <- ggplot(Boxplot_cobertura_Cadastro2,
                aes(x = Porte_Mun, y = value, fill = Cadastro_Cobertura,
                    group = Cadastro_Cobertura)) +
      geom_boxplot() +
      ylim(0,1)

    g_plotly <- ggplotly(g) %>% layout(boxmode = "group",
                                       xaxis = list(title = 'Porte'),
                                       yaxis = list(title = 'Percentage (%)'),
                                       legend = list(title = list(text = '<b> Cadastro (Q3-2019)
                                                                X Cobertura (Q2-2020) </b>'))) %>%
                                config(
                                        toImageButtonOptions = list(
                                        format = "svg",
                                        filename = "myplot",
                                        width = 600,
                                        height = 700) )
    g_plotly

  })
  
#Tab 4####

#10. Repasses####
Repasses <-  reactive({
    
    minpopsize <- input$popsize[1]
    maxpopsize <- input$popsize[2]
   
  # Apply filters
  r <- Repasses_Previne %>%
    filter(SISAB_C_POP.MUN >= minpopsize,
           SISAB_C_POP.MUN <= maxpopsize) %>%
    arrange(group)
  
  # Optional: filter by group
  if (input$group != "All") {
    Group <- paste0(input$group)
    r <- r %>% filter(group == Group)
  }
  
  # Optional: filter by region
  if (input$region != "All") {
    Region <- paste0(input$region)
    r <- r %>% filter(IBGE_NO_G_Reg == Region)
  }
  # Optional: filter by Tipologia
  if (input$tipologia != "All") {
    Tipologia <- paste0(input$tipologia)
    r <- r %>% filter(Tipologia_C_50 == Tipologia)
  }
  # Optional: filter by Capital
  if (input$capital != "All") {
    Capital <- paste0(input$capital)
    Capital_B <- ifelse(Capital == "Yes", 1,0)
    r <- r %>% filter(capital == Capital_B)
  }
  
  # Optional: filter by Recebe_PerCapita_de_Trans
  if (input$fns_65584 != "All") {
    fns_65584 <- paste0(input$fns_65584)
    fns_65584_B <- ifelse(fns_65584 == "Yes", 1,0)
    r <- r %>% filter(Recebe_PerCapita_de_Trans == fns_65584_B)
  }
  
  # Optional: filter by Recebe_Fator_Comp_de_Trans
  if (input$fns_65586 != "All") {
    fns_65586 <- paste0(input$fns_65586)
    fns_65586_B <- ifelse(fns_65586 == "Yes", 1,0)
    r <- r %>% filter(Recebe_Fator_Comp_de_Trans == fns_65586_B)
  }
  
  # Optional: filter by Municipio
  if (!is.null(input$municipio) && input$municipio != "") {
    Municipio <- toupper(paste0(input$municipio))
    r <- r %>% filter(NO_MUNICIPIO %like% Municipio)
  }
  # Optional: filter by Estado
  if (!is.null(input$estado) && input$estado != "") {
    Estado <- str_to_title(paste0(input$estado))
    r <- r %>% filter(IBGE_NO_UF %like% Estado)
  }
  
  r <- as.data.frame(r)
  
  
})
  
###10.1 Putput - EvolucaoRepasses####
output$EvolucaoRepasses <- renderPlotly({
    
    Rep <- Repasses() %>% 
      filter(Categoria == "Capitação ponderada") %>%
      group_by(date, acao.y) %>% summarise(Vlacao = sum(Vlacao))
    
    grepasse <- ggplot(Rep, 
                 aes(x = date, y = Vlacao,
                     fill = acao.y, group = acao.y)) 
  

    
    grepasse <- grepasse + geom_col( show.legend = FALSE) + 
      #facet_wrap(~Categoria) + 
      theme(legend.position = 'none') +
      xlab("Período") +
      ylab("Repasse") +
      scale_y_continuous(labels = addUnits)
  
  
  kk <- ggplotly(grepasse) %>%
    config(
      toImageButtonOptions = list(
        format = "svg",
        filename = "myplot",
        width = 600,
        height = 700) )
  kk
  })
  
#Tab 5####
  
##10. Dictionary####
  output$dict <- renderDataTable({
    Previne_Dicionario
  })
}