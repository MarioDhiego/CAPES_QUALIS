
library(shiny)
library(DT)
library(shinyWidgets)
library(shinydashboard)
library(tidyverse)

qualis_capes <- readr::read_rds("output/qualis_capes.rds")

ui <- dashboardPage(skin = "blue",
dashboardHeader(title = "PESQUISA DE PERIÓDICOS",
                titleWidth = 300),
dashboardSidebar(width = 300,
sidebarMenu(
  menuItem("Tipos de Periódicos", 
           tabName = "dashboard", 
           icon = icon("search")),
  menuItem("Saiba Mais", 
           tabName = "sobre", 
           icon = icon("graduation-cap")),
  menuItem("Acesse o Script", 
           href = "https://github.com/MarioDhiego/CAPES_QUALIS",
           icon = icon("github-square")
           )
  )
),
dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", 
              type = "text/css", 
              href = "custom.css")),
  tabItems(
    tabItem(tabName = "dashboard",
"Este Projeto foi elaborado com o objetivo de facilitar a Pesquisa de Periódicos Científicos.", 
br(), 
br(),
"É possível consultar as áreas de avaliação dos programas de pós-graduação no site da", 
tags$a(href = "http://avaliacaoquadrienal.capes.gov.br/resultado-da-avaliacao-quadrienal-2017-2", "CAPES"),
".", 
br(), 
br(),
fluidRow( 
  box(
    selectizeInput(
      inputId = "area_de_avaliacao_i",
      label = "Área(s) de Avaliação",
      choices =  sort(unique(qualis_capes$area_de_avaliacao)), 
      multiple = TRUE,
      selected = FALSE
      )
    ), 
  box(
    sliderTextInput(
      inputId = "estrato_i",
      label = "Classificação do Periódico",
      choices = sort(unique(qualis_capes$estrato)),
      selected = c("A1", "C"),
      grid = TRUE )
  ) 
),
valueBoxOutput("n_total_periodicos", 
               width = 6), 
valueBoxOutput("n_periodicos_filtrados", 
               width = 6), 
DTOutput("tabela_periodicos")
),
tabItem(tabName = "sobre",
        h2("Sobre esse Shiny App"),
"- Essa página foi elaborada com a intenção de facilitar 
a pesquisa de periódicos para publicação em grupos multi e interdisciplinares. 
Os dados foram obtidos no website da ",
tags$a(href = "https://sucupira.capes.gov.br/sucupira/public/consultas/coleta/veiculoPublicacaoQualis/listaConsultaGeralPeriodicos.jsf", "CAPES"),
".",
br(),
br(),
"- Desenvolvido utilizando:",
tags$a(href = "https://www.r-project.org/", "R"), br(),
tags$a(href = "https://www.rstudio.com/", "RStudio"), br(),
tags$a(href = "https://shinyapps.io", "Shinyapps.io"), br(),
"e os pacotes",
tags$a(href = "https://shiny.rstudio.com/", "shiny"), ",",
tags$a(href = "https://rstudio.github.io/shinydashboard/", "shinydashboard"), ",",
tags$a(href = "https://dreamrs.github.io/shinyWidgets/index.html", "shinyWidgets"), ",",
tags$a(href = "https://rstudio.github.io/DT/", "DT"), ",",
tags$a(href = "https://www.tidyverse.org/", "tidyverse"), ",",
tags$a(href = "https://www.tidyverse.org/", "dplyr"), ",",
tags$a(href = "https://purrr.tidyverse.org", "purrr"), "e",
tags$a(href = "https://stringr.tidyverse.org/", "stringr"), ".",
br(),
br(),
"- Desenvolvido por",tags$a(href = "https://github.com/MarioDhiego", "Mário Dhiego"),
                          )
                        )
                      )
)


server <- function(input, output) {
  filtra_varios <- function(data, lista) {
    purrr::reduce(lista, ~{
      filter(.x, stringr::str_detect(area_conc, .y))
    }, .init = data)
  }
  filtered_data <- reactive({
    req( input$area_de_avaliacao_i)
    qualis_capes2 <- qualis_capes %>%
      filter(as.character(estrato) >= as.character(input$estrato_i[1]), 
             as.character(estrato) <= as.character(input$estrato_i[2])) %>% 
      group_by(issn) %>% 
      mutate(area_conc = paste(area_de_avaliacao, collapse = " - ")) %>% 
      ungroup() %>% 
      filtra_varios(input$area_de_avaliacao_i) %>% 
      filter(area_de_avaliacao %in% input$area_de_avaliacao_i)  %>% 
      arrange(issn)
    qualis_capes2
  })
  
  output$n_total_periodicos <- renderValueBox({
    valueBox(
      value = length(unique(qualis_capes$issn)),
      subtitle =  "Número de periódicos cadastrados",
      icon = icon("fas fa-book"), #icone nao ta funcionando
      color = "aqua"
    )
  })
  
  output$n_periodicos_filtrados <- renderValueBox({
    valueBox(
      value = length(unique(filtered_data()$issn)),
      subtitle =  "Número de periódicos filtrados",
      icon = icon("fas fa-search"), 
      color = "green"
    )
  })
  
  output$tabela_periodicos <- renderDT({
    filtered_data()  %>%
      select(-area_conc) %>% 
      mutate(titulo = 
               paste0("<a href='https://www.google.com/search?q=issn%20",
                issn, "%20-%20", titulo, "', target='_blank' />", titulo, "</a>")) %>% 
      rename(ISSN = issn,
             `Título do Periódico` = titulo,
             `Área de Avaliação` = area_de_avaliacao,
             `Estrato CAPES` = estrato#,
      ) 
  } , escape = FALSE)
}
shinyApp(ui, server)