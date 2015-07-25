library(shiny)

shinyUI(fluidPage(
  includeCSS("style.css"),
  # tytuł
  headerPanel("Wyniki matur 2014 - histogramy"),
  sidebarPanel(    
    selectInput('przedmiot', 'Przedmiot',
                c("biologia", "chemia", "fizyka", "geografia", "historia", 
                  "informatyka", "j. angielski", "j. polski", "matematyka", "WOS"),
                selected="j. polski"),
    
    selectInput('poziom', 'Poziom', c("podstawowa", "rozszerzona"),
                selected="podstawowa"),
    
    selectInput('typHist', 'Typ histogramu', c("frekwencja", "liczebność")),
    
    uiOutput("kategoriaSelektor"),
    uiOutput("filtrSelektor"),
    uiOutput("wartoscSelektor"),
    
    HTML("Marta Czarnocka-Cieciura i Piotr Migdał <br>
     <a href=\"https://github.com/stared/delab-matury\">https://github.com/stared/delab-matury</a>")
  ),

  mainPanel(
    # wyższe wykresy
    plotOutput("ggHistMatury", height=600)
  )
))
    