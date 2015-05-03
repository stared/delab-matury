library(shiny)
library(ggplot2)
library(dplyr)

# histogram bez podziału na grupy
ggHistWszyscy<-function(nazwa) {
  sum_wynik <- dane[,nazwa]
  procent_wynik <- data.frame(100 * sum_wynik/max(sum_wynik, na.rm=T))
  names(procent_wynik) <- c("procent_wynik")
  krok <- 100 * 1/max(sum_wynik,  na.rm=T)
  p <- ggplot(procent_wynik, aes(x=procent_wynik, y = ..density.. * 100)) +
    geom_histogram(colour="white", binwidth=krok) +
    xlab("% punktów") +
    ylab("% uczniów") +
    ggtitle(gsub("^j_", "j. ", nazwa) %>% gsub("_", " ", .)) 
  return(p)
}

# histogram z podziałem na grupy
ggHistPodzial<-function(nazwa, filtr, tytul_legendy=filtr, zamienNA=NA, kolory=c("red", "blue")){
  dane_zmod <- dane
  if (is.na(zamienNA)) {
    # usuwa wiersze z NA w kolumnie filtr
    dane_zmod <- dane_zmod[!is.na(dane_zmod[,filtr]),]
  }
  else{
    # zamienia NA w kolumnie filtr na 'zmianNA'
    dane_zmod[is.na(dane_zmod[,filtr]), filtr] <- zmienNA 
  }
  sum_wynik <- dane_zmod[nazwa]  
  procent_wynik <- 100 * sum_wynik/max(sum_wynik, na.rm=T)
  filtr_dane <- dane_zmod[,filtr]
  dane_wybrane <- data.frame(procent_wynik, filtr_dane)
  colnames(dane_wybrane) <- c("procent_wynik", "filtr_dane")
  krok <- 100 * 1/max(sum_wynik,  na.rm=T)
  p <- ggplot(dane_wybrane, aes(x=procent_wynik, fill=filtr_dane, y=..density.. * 100)) +
    scale_fill_manual(values=kolory, name=filtr_dane) +
    geom_histogram(binwidth=krok, binwidth=.5, alpha=.3, position="identity") +
    xlab("% punktów") +
    ylab("% uczniów") +
    guides(fill=guide_legend(title=tytul_legendy)) +
    ggtitle(gsub("^j_", "j. ", nazwa) %>% gsub("_", " ", .))   
  return(p)
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # pokazuje progress przy obliczeniach
  withProgress(message = 'Wczytuję wyniki,',
               detail = 'Może chwilę potrwać...', value = 0, {
                 dane <- read.csv("../../dane/przetworzone/sumy_laureaty.csv")
               })
  
  #dane <- read.csv("../../dane/przetworzone/sumy_laureaty.csv")
  
  output$ggHistMatury <- renderPlot({
    nazwa <- paste(input$przedmiot, input$poziom) %>%
      gsub("\\.* ", "_", .)
    if (input$podzial == "--"){
     # cat("--", file = stderr())
      wykres <- ggHistWszyscy(nazwa)
    }
    if (input$podzial == "płeć"){
      wykres <- ggHistPodzial(nazwa, 'plec', tytul_legendy="płeć")
    }
    if (input$podzial == "dysleksja"){
      wykres <- ggHistPodzial(nazwa, 'dysleksja')
    }
    wykres
  })
  
})
