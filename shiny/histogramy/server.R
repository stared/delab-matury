library(shiny)
library(ggplot2)
library(gridExtra)
library(dplyr)

# Todo:
# równa szerokość wykresów
# wykres liczebności grup: dla podziału tylko z wybranej kategorii?
# dane z wcześniejszych lat

shinyServer(function(input, output, session) {
  # pokazuje progress przy obliczeniach
  withProgress(message = 'Wczytuję wyniki,',
               detail = 'Może chwilę potrwać...', value = 0, {
                 #Sys.sleep(600)
                 dane <- read.csv("wyniki2014.csv")
                 # posortowanie rocznikow
#                  grupyWiekowe <- c(paste(rok - najliczniejszy - 1, "i mniej"),
#                                    paste(rok - najliczniejszy),
#                                    paste(rok - najliczniejszy + 1),
#                                    paste(rok - najliczniejszy + 2, "i więcej"))
#                  daneRocznik$wiek <- factor(daneRocznik$wiek, levels = grupyWiekowe)
                 
                 # liczebnosci poszczegolnych grup
                 grupyWiekowe <-unique(dane$wiek[!is.na(dane$wiek)])
                 colnames(dane)[colnames(dane)=="rodzaj_gminy"] <- "rodzaj gminy"
                 colnames(dane)[colnames(dane)=="typ_szkoly"] <- "typ szkoły"
                 colnames(dane)[colnames(dane)=="publiczna"] <- "szkoła publiczna?"
                 colnames(dane)[colnames(dane)=="wielkosc_miejscowosci"] <- "wielkość miejscowości"
                 kategoria <- c("płeć", "płeć",
                                "dysleksja", "dysleksja",
                                rep("wiek", length(grupyWiekowe)),
                                rep("typ szkoły", 3),
                                rep("szkoła publiczna?", 2),
                                rep("rodzaj gminy", 3),
                                rep("wielkość miejscowości", 3))
  
                 grupa <- c("kobiety", "mężczyźni",
                            "brak dysleksji", "dysleksja",
                            "18 i mniej", "19", "20", "21 i więcej",
                            "liceum ogólnokształcące", "liceum profilowane", "technikum",
                            "publiczna", "prywatna",
                            "wiejska", "miejsko-wiejska", "miejska",
                            "poniżej 5 tys.", "5 tys. - 50 tys.", "ponad 50 tys."
                            )

                filtr_pusty <- "wybierz filtr!"
                output$kategoriaSelektor <- renderUI({
                  selectInput("podzial", "Podział", choices=as.list(c("--", unique(kategoria))), selected="--") 
                })
                output$filtrSelektor <- renderUI({
                  selectInput("filtr", "Filtr", choices=as.list(c("--", unique(kategoria))), selected="--") 
                })
                output$wartoscSelektor <- renderUI({
                  selectInput("wartosc", "Wartość filtra", choices=as.list(filtr_pusty), selected=filtr_pusty) 
                })              
               }) 
  

  # histogram bez podziału na grupy
  ggHistWszyscy<-function(nazwa, filtr="--", wartosc=filtr_pusty) {
    tytul <- gsub("^j_", "j. ", nazwa) %>% gsub("_", " ", .)
    if (filtr=="--" | !(wartosc %in% grupa[kategoria==filtr])){
      dane_zmod <- dane
    }
    else{
      dane_zmod <- dane[dane[ ,filtr]==wartosc,]
      tytul <- paste(tytul, "\n", filtr, ": ", wartosc, sep="")
    }
    sum_wynik <- dane_zmod[,nazwa]
    procent_wynik <- data.frame(100 * sum_wynik/max(sum_wynik, na.rm=T))
    names(procent_wynik) <- c("procent_wynik")
    krok <- 100 * 1/max(sum_wynik,  na.rm=T)
    p <- ggplot(procent_wynik, aes(x=procent_wynik, y = ..density.. * 100)) +
      geom_histogram(color="white", binwidth=krok) +
      xlab("% punktów") +
      ylab("% zdających") +
      ggtitle(tytul) 
    return(p)
  }
  
#   # histogram z podziałem na grupy
  ggHistPodzial<-function(nazwa, podzial, filtr="--", wartosc=filtr_pusty, tytul_legendy=podzial, kolory=c("red", "blue")){
    tytul <- gsub("^j_", "j. ", nazwa) %>% gsub("_", " ", .)
    if (filtr=="--" | !(wartosc %in% grupa[kategoria==filtr])){
      dane_zmod <- dane
      tytul <- paste(tytul, "\nz podziałem na", podzial)
    }
    else{
      dane_zmod <- dane[dane[ ,filtr]==wartosc, ]
      tytul <- paste(tytul, "\n", filtr, ": ", wartosc, " z podziałem na ", podzial, sep="")
    }
    sum_wynik <- dane_zmod[,nazwa]  
    procent_wynik <- 100 * sum_wynik/max(sum_wynik, na.rm=T)
    podzial_dane <- dane_zmod[,podzial]
    dane_wybrane <- data.frame(procent_wynik, podzial_dane)
    dane_wybrane <- dane_wybrane[complete.cases(dane_wybrane),]
    colnames(dane_wybrane) <- c("procent_wynik", "podzial_dane")
    krok <- 100 * 1/max(sum_wynik,  na.rm=T)
    p <- ggplot(dane_wybrane, aes(x=procent_wynik, fill=podzial_dane, y=..density.. * 100)) +
      scale_fill_manual(values=kolory, name=podzial_dane) +
      geom_histogram(binwidth=krok, binwidth=.5, alpha=.3, position="identity") +
      xlab("% punktów") +
      ylab("% zdających w ramach grupy") +
      guides(fill=guide_legend(title=tytul_legendy)) +
      ggtitle(tytul)   
    return(p)
  }   

  grupyWykres <- function(nazwa, filtr="--", wartosc=filtr_pusty){
    gg_color_hue <- function(n) {
      hues = seq(15, 375, length=n+1)
      hcl(h=hues, l=65, c=100)[1:n]
    }
    kolory <- gg_color_hue(length(unique(kategoria)))
    if (filtr=="--" | !(wartosc %in% grupa[kategoria==filtr])){
      kategoria_zmod <- kategoria
      grupa_zmod <- grupa
      dane_zmod <- dane
      tytul <- "kto zdaje?"
    }
    else{
      kategoria_zmod <- kategoria[kategoria!=filtr]
      grupa_zmod <- grupa[kategoria!=filtr]
      dane_zmod <- dane[dane[,filtr]==wartosc,]
      tytul <- paste("kto zdaje?", "\n", filtr, ": ", wartosc, sep="")
      kolory<-kolory[unique(kategoria)!=filtr]
    }
    #liczebnosc grup
    liczba <- sapply(1:length(grupa_zmod), function(i){
      k <- kategoria_zmod[i]
      g <- grupa_zmod[i]
      return(length(which(dane_zmod[, k] == g & !is.na(dane_zmod[,nazwa]))))
    })
    
    liczba <- liczba/1000
    
    liczebnosc_grup <- data.frame(kategoria_zmod, grupa_zmod, liczba)
    liczebnosc_grup$grupa_zmod = factor(liczebnosc_grup$grupa, levels=rev(grupa_zmod))
    liczebnosc_grup$kategoria_zmod = factor(liczebnosc_grup$kategoria, levels=unique(kategoria_zmod)) #c("płeć", "dysleksja", "poprawkowa", "wiek"))
    
#     cat(str(liczebnosc_grup), file = stderr())
#     cat("\n", file = stderr())
    grupHist <- ggplot(liczebnosc_grup, aes(x=grupa_zmod, y=liczba, fill=kategoria_zmod)) +
      geom_bar(position=position_dodge(width=0.8), alpha=0.7, stat='identity') +
      scale_fill_manual(values=kolory) +
      xlab("") +
      ylab("liczba zdających (w tysiącach)") +
      guides(fill=guide_legend(title="kategorie")) +
      coord_flip() +
      ggtitle(tytul)
    return(grupHist)
  }

observe({ 
  if (is.null(input$filtr)){
    filtr <- "--"
  }
  else{
    filtr <- input$filtr
  }
  if (filtr=="--"){
    updateSelectInput(session, "wartosc", "Wartość filtra", choices="wybierz filtr!")
  }
  else {
    updateSelectInput(session, "wartosc", "Wartość filtra", choices=as.list(grupa[kategoria==input$filtr]))
  }
  
#   var.opts<-namel(colnames(obj))
#   var.opts.original.slicers <- namel(colnames(TestData))           
#   measures <- c('m1','m2','m3','m4','m5')
#   
#   var.opts.slicers <- var.opts[!(var.opts %in% c(measures,'x'))]
#   var.opts.original.slicers <- var.opts.original.slicers[!(var.opts.original.slicers %in% c(measures,'x'))]
#   var.opts.measures <- var.opts[var.opts %in% measures]
#   
#   updateSelectInput(session, "source_columns", choices = var.opts.original.slicers, selected=var.opts.slicers)
#   updateSelectInput(session, "xaxis", choices = var.opts.slicers,selected="x")
  
})

  
  output$ggHistMatury <- renderPlot({
    #cat("start \n", file = stderr())
    nazwa <- paste(input$przedmiot, input$poziom) %>%
      gsub("\\.* ", "_", .)
    if (is.null(input$wartosc)){
      wartosc <- filtr_pusty
    }
    else{
      wartosc <- input$wartosc
    }
    if (is.null(input$podzial)){
      podzial <- "--"
    }
    else{
      podzial <- input$podzial
    }
    if (is.null(input$filtr)){
      filtr <- "--"
    }
    else{
      filtr <- input$filtr
    }
#     
#     if (filtr!="--"){
#       output$wartoscSelektor <- renderUI({
#         selectInput("wartosc", "Wartość filtra", choices=as.list(grupa[kategoria==input$filtr]), selected=wartosc)
#       })
#     }
#     
    
    if (podzial == "--"){
#       cat(nazwa, file = stderr())
#       cat(filtr, file = stderr())
#       cat(wartosc, file = stderr())
#       cat("\n", file = stderr())
      wykres <- ggHistWszyscy(nazwa, filtr, wartosc)
    }
    if (podzial == "wiek"){
      wykres <- ggHistPodzial(nazwa, 'wiek', filtr, wartosc, kolory = c("green", "blue", "red", "black"))
    }
    if (podzial %in% c("rodzaj gminy", "wielkość miejscowości", "typ szkoły")){
      wykres <- ggHistPodzial(nazwa, podzial, filtr, wartosc, kolory = c("green", "blue", "red"))
    }
#     if (podzial == "wielkość miejscowości", "typ szkoły"){
#       wykres <- ggHistPodzial(nazwa, 'wielkość miejscowości', filtr, wartosc, kolory = c("green", "blue", "red"))
#     } 
    if (podzial %in% c( "płeć", "dysleksja", "szkoła publiczna?")) {
      wykres <- ggHistPodzial(nazwa, input$podzial, filtr, wartosc)
    }
    
    grupHist <- grupyWykres(nazwa, filtr, wartosc)
    
    multi <- arrangeGrob(wykres, grupHist)#, sub = textGrob("Piotr Migdał, Marta Czarnocka-Cieciura, https://github.com/stared/delab-matury",
    #                                                    x = 0, hjust = -0.1, vjust=0.1,
    #                                                    gp = gpar(fontsize = 9)))
    print(multi)
    #wykres
    #grupHist

  })
  
})
