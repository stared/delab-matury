library(shiny)
library(ggplot2)
library(gridExtra)
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
                 dane$plec <- factor(dane$plec, levels=c("k", "m"), labels=c("kobiety", "mężczyźni"))
                 names(dane)[names(dane) == 'plec'] <- 'płeć'
                 dane$dysleksja <- factor(dane$dysleksja, levels=c(FALSE, TRUE), labels=c("nie", "tak"))
                 
                 # przetwarzanie danych o wieku
                 daneRocznik <- dane
                 rok <- 2014 # do zmiany, jesli beda dane z roznych lat
                 # wycinam dziwne przypadki z przed 1900 roku i po 2014 roku oraz osoby z NA zamiast rocznika
                 daneRocznik <- daneRocznik[!daneRocznik$rocznik>2014 & !daneRocznik$rocznik<1900 & !is.na(daneRocznik$rocznik),]
                 najliczniejszy <- as.numeric(names(which.max(table(daneRocznik$rocznik))))
                 daneRocznik$wiek[daneRocznik$rocznik == najliczniejszy] <- rok - najliczniejszy
                 daneRocznik$wiek[daneRocznik$rocznik > najliczniejszy] <- paste(rok - najliczniejszy - 1, "i mniej")
                 daneRocznik$wiek[daneRocznik$rocznik < najliczniejszy & daneRocznik$rocznik >= (najliczniejszy - 1)] <- paste(rok - najliczniejszy + 1)
                 daneRocznik$wiek[daneRocznik$rocznik < najliczniejszy - 1] <-  paste(rok - najliczniejszy + 2, "i więcej")
                 
                 # posortowanie rocznikow
                 grupyWiekowe <- c(paste(rok - najliczniejszy - 1, "i mniej"),
                                   paste(rok - najliczniejszy),
                                   paste(rok - najliczniejszy + 1),
                                   paste(rok - najliczniejszy + 2, "i więcej"))
                 daneRocznik$wiek <- factor(daneRocznik$wiek, levels = grupyWiekowe)
                 
                 # liczebnosci poszczegolnych grup
                 kategoria <- c("płeć", "płeć",
                                "dysleksja", "dysleksja",
                                "wiek", "wiek", "wiek", "wiek")
                 grupa <- c("kobiety", "mężczyźni",
                            "brak dysleksji", "dysleksja",
                            grupyWiekowe[1], grupyWiekowe[2], grupyWiekowe[3], grupyWiekowe[4])
                               
               })  
  
  # histogram bez podziału na grupy
  ggHistWszyscy<-function(nazwa) {
    sum_wynik <- dane[,nazwa]
    procent_wynik <- data.frame(100 * sum_wynik/max(sum_wynik, na.rm=T))
    names(procent_wynik) <- c("procent_wynik")
    krok <- 100 * 1/max(sum_wynik,  na.rm=T)
    p <- ggplot(procent_wynik, aes(x=procent_wynik, y = ..density.. * 100)) +
      geom_histogram(color="white", binwidth=krok) +
      xlab("% punktów") +
      ylab("% uczniów") +
      ggtitle(gsub("^j_", "j. ", nazwa) %>% gsub("_", " ", .)) 
    return(p)
  }
  
#   # histogram z podziałem na grupy
  ggHistPodzial<-function(nazwa, filtr, data=dane, tytul_legendy=filtr, kolory=c("red", "blue")){
    dane_zmod <- data
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
      wykres <- ggHistPodzial(nazwa, "płeć")
    }
    if (input$podzial == "dysleksja"){
      wykres <- ggHistPodzial(nazwa, 'dysleksja')
    }
    if (input$podzial == "wiek"){
      wykres <- ggHistPodzial(nazwa, 'wiek', data=daneRocznik, kolory = c("green", "blue", "red", "black"))
    }
    
    #liczebnosc grup
    liczba <- c(
      length(which(dane$płeć == "kobiety" & !is.na(dane[,nazwa]))),
      length(which(dane$płeć == "mężczyźni" & !is.na(dane[,nazwa]))),
      length(which(dane$dysleksja == "nie" & !is.na(dane[,nazwa]))),
      length(which(dane$dysleksja == "tak" & !is.na(dane[,nazwa]))),
      length(which(daneRocznik$wiek == grupyWiekowe[1] & !is.na(daneRocznik[,nazwa]))),
      length(which(daneRocznik$wiek == grupyWiekowe[2] & !is.na(daneRocznik[,nazwa]))),
      length(which(daneRocznik$wiek == grupyWiekowe[3] & !is.na(daneRocznik[,nazwa]))),
      length(which(daneRocznik$wiek == grupyWiekowe[4] & !is.na(daneRocznik[,nazwa])))
    )
    
    liczba <- liczba/1000
    
    liczebnosc_grup <- data.frame(kategoria, grupa, liczba)
    liczebnosc_grup$grupa = factor(liczebnosc_grup$grupa, levels=rev(grupa))
    liczebnosc_grup$kategoria = factor(liczebnosc_grup$kategoria, levels=c("płeć", "dysleksja", "poprawkowa", "wiek"))
    
    grupHist <- ggplot(liczebnosc_grup, aes(x=grupa, y=liczba, fill=kategoria)) +
      geom_bar(position=position_dodge(width=0.8), alpha=0.7, stat='identity') +
      xlab("") +
      ylab("liczba zdających (w tysiącach)") +
      coord_flip() +
      ggtitle("kto zdaje?")
    
    multi <- arrangeGrob(wykres, grupHist, sub = textGrob("Piotr Migdał, Marta Czarnocka-Cieciura, https://github.com/stared/delab-matury",
                                                        x = 0, hjust = -0.1, vjust=0.1,
                                                        gp = gpar(fontsize = 9)))
    multi
    #wykres
  })
  
})
