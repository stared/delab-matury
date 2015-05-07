library(shiny)
library(ggplot2)
library(dplyr)

# Todo:
# legenda/wykres: ile uczniów zdaje z podziałem na kategorie
# równa szerokość wykresów

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # pokazuje progress przy obliczeniach
  withProgress(message = 'Wczytuję wyniki,',
               detail = 'Może chwilę potrwać...', value = 0, {
                 dane <- read.csv("../../dane/przetworzone/sumy_laureaty.csv")
               })  
  
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
  ggHistPodzial<-function(nazwa, filtr, data=dane, tytul_legendy=filtr, zamienNA=NA, kolory=c("red", "blue")){
    dane_zmod <- data
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
  
  output$ggHistMatury <- renderPlot({
    #cat("start \n", file = stderr())
    nazwa <- paste(input$przedmiot, input$poziom) %>%
      gsub("\\.* ", "_", .)
    if (input$podzial == "--"){
      wykres <- ggHistWszyscy(nazwa)
    }
    if (input$podzial == "płeć"){
      wykres <- ggHistPodzial(nazwa, 'plec', tytul_legendy="płeć")
    }
    if (input$podzial == "dysleksja"){
      wykres <- ggHistPodzial(nazwa, 'dysleksja')
    }
    if (input$podzial == "wiek"){
      daneRocznik <- dane
      rok <- 2014 # do zmiany, jesli beda dane z roznych lat
      # wycinam dziwne przypadki z przed 1900 roku i po 2014 roku oraz osoby z NA zamiast rocznika
      daneRocznik <- daneRocznik[!daneRocznik$rocznik>2014 & !daneRocznik$rocznik<1900 & !is.na(daneRocznik$rocznik),]
      najliczniejszy <- as.numeric(names(which.max(table(daneRocznik$rocznik))))
      daneRocznik$wiek[daneRocznik$rocznik == najliczniejszy] <- rok - najliczniejszy
      daneRocznik$wiek[daneRocznik$rocznik > najliczniejszy] <- paste(rok - najliczniejszy - 1, "i mniej")
      daneRocznik$wiek[daneRocznik$rocznik < najliczniejszy & daneRocznik$rocznik >= (najliczniejszy - 5)] <- paste(rok - najliczniejszy + 1, '-', rok - najliczniejszy + 5)
      daneRocznik$wiek[daneRocznik$rocznik < najliczniejszy - 5] <-  paste(rok - najliczniejszy + 6, "i więcej")
      
      # posortowanie rocznikow
      daneRocznik$wiek <- factor(daneRocznik$wiek, levels = c(paste(rok - najliczniejszy - 1, "i mniej"),
                                                              paste(rok - najliczniejszy),
                                                              paste(rok - najliczniejszy + 1, '-', rok - najliczniejszy + 5),
                                                              paste(rok - najliczniejszy + 6, "i więcej")))
      wykres <- ggHistPodzial(nazwa, 'wiek', data=daneRocznik, kolory = c("green", "blue", "red", "black"))
    }
    wykres
  })
  
})
