library(shiny)

shinyUI(pageWithSidebar(
  # tytuł
  headerPanel("Wyniki matur - histogramy"),
  sidebarPanel(
    selectInput('przedmiot', 'Przedmiot',
                c("biologia", "chemia", "fizyka", "geografia", "historia", 
                  "informatyka", "j. angielski", "j. polski", "matematyka", "WOS"),
                selected="j. polski"),
    
    selectInput('poziom', 'Poziom', c("podstawowa", "rozszerzona"),
                selected="podstawowa"),
    
    selectInput('podzial', 'Podział', c("--", "płeć", "dysleksja"),
                selected="--")
  ),

  mainPanel(
    plotOutput("ggHistMatury")
  )
))