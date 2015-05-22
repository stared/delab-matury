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
    
    #selectInput('podzial', 'Podział', c("--", "płeć", "dysleksja", "wiek", "typ szkoły", "publiczna", "rodzaj gminy", "wielkość miejscowości"),
    #            selected="--"),
    
    #cat( uiOutput("kategoriaSelektor"), file = stderr()),
    uiOutput("kategoriaSelektor"),
    uiOutput("filtrSelektor"),
    uiOutput("wartoscSelektor"),
    
    HTML("Piotr Migdał i Marta Czarnocka-Cieciura, \n
     <a href=\"https://github.com/stared/delab-matury\">https://github.com/stared/delab-matury</a>")
  ),

  mainPanel(
    # wyższe wykresy
    plotOutput("ggHistMatury", height=600)
  )
))
    