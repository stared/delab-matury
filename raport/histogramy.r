# histogramy - funkcje z shiny
library(ggplot2)
library(gridExtra)
library(dplyr)

dane <- read.csv("../shiny/histogramy/wyniki2014.csv")
grupyWiekowe <-unique(dane$wiek[!is.na(dane$wiek)])
colnames(dane)[colnames(dane)=="rodzaj_gminy"] <- "rodzaj gminy"
colnames(dane)[colnames(dane)=="typ_szkoly"] <- "typ szkoły"
colnames(dane)[colnames(dane)=="publiczna"] <- "szkoła publiczna?"
colnames(dane)[colnames(dane)=="wielkosc_miejscowosci"] <- "wielkość miejscowości"
kategoria <- c("płeć", "płeć",
               "dysleksja", "dysleksja",
               rep("wiek", length(grupyWiekowe)),
               rep("typ szkoły", 2),
               rep("szkoła publiczna?", 2),
               rep("rodzaj gminy", 3),
               rep("wielkość miejscowości", 3))

grupa <- c("kobiety", "mężczyźni",
           "brak dysleksji", "dysleksja",
           "18 i mniej", "19", "20", "21 i więcej",
           "liceum", "technikum",
           "publiczna", "prywatna",
           "wiejska", "miejsko-wiejska", "miejska",
           "poniżej 5 tys.", "5 tys. - 50 tys.", "ponad 50 tys."
)

filtr_pusty <- ""


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
    tytul <- paste(tytul, "\nz podziałem na:", podzial)
  }
  else{
    dane_zmod <- dane[dane[ ,filtr]==wartosc, ]
    tytul <- paste(tytul, "\n", filtr, ": ", wartosc, " z podziałem na: ", podzial, sep="")
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
