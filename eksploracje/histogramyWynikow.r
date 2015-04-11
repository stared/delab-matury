library(ggplot2)
library(gridExtra)

bazowaSciezka <- "dane/wyniki/"

# funkcja tworząca histogram z wyników pobranych z pliku 
# wyniki muszą być wcześniej pobrane i zapisane w bazowaSciezka 
# w formacie: typ_matury_rok.csv ("_" zamiast spacji w typie matury)
# np. przez skrypt sciaganieWynikow.r
# skrypt powinien być uruchamiany z folderu, w którym jest repozytorium, a nie sam skrypt.

ggHistMatury<-function(typ_matury, rok){
  file_name <- paste(paste(gsub(" ", "_", typ_matury), rok, sep="_"), "csv", sep=".")
  sciezka <- paste(bazowaSciezka, file_name, sep = '')
  print(paste("wczytuję dane z pliku ", sciezka, ", może chwilę potrawć", sep = ''))
  wynik <- read.csv(sciezka, stringsAsFactors=F)
  pytania <- wynik[,grep("^k_[0-9]*", names(wynik))]
  sum_wynik <- rowSums(pytania, na.rm=T)
  procent_wynik <- 100 * sum_wynik/max(sum_wynik, na.rm=T)
  krok <- 100 * 1/max(sum_wynik,  na.rm=T)
        
  ggplot(data.frame(procent_wynik), aes(x=procent_wynik)) +
    geom_histogram(colour="white", binwidth=krok) +
    xlab("% punktów") +
    ylab("liczba uczniów") +
    ggtitle(paste("Wyniki matury ", typ_matury, " w ", rok, "roku"))
}

# przykład
ggHistMatury("matematyka podstawowa", 2014)

## z rozbiciem na podgrupy

# pobranie pliku
file_name <- paste(paste(gsub(" ", "_", typ_matury), rok, sep="_"), "csv", sep=".")
sciezka <- paste(bazowaSciezka, file_name, sep = '')
print(paste("wczytuję dane z pliku ", sciezka, ", może chwilę potrawć", sep = ''))
wynik <- read.csv(sciezka, stringsAsFactors=F)
nazwyKolumn <- names(wynik)

ggHistMatury2<-function(wynik, info){
  pytania <- wynik[,grep("^k_[0-9]*", nazwyKolumn)]
  sum_wynik <- rowSums(pytania, na.rm=T)
  procent_wynik <- 100 * sum_wynik/max(sum_wynik, na.rm=T)
  krok <- 100 * 1/max(sum_wynik,  na.rm=T)
  return( ggplot(data.frame(procent_wynik), aes(x=procent_wynik)) +
    geom_histogram(aes(y = ..density..), colour="white", binwidth=krok) +
    xlab("% punktów") +
    ylab("liczba uczniów") +
    ggtitle(info) )
}

wynikK<-wynik[!is.na(wynik$plec) & wynik$plec == 'k',]
wykresK <- ggHistMatury2(wynikK, 'plec = k')

wynikM <- wynik[!is.na(wynik$plec) & wynik$plec == 'm',]
wykresM <- ggHistMatury2(wynikM, 'plec = m')

grid.arrange(wykresK, wykresM, ncol=2)

# dysleksja
wynikK<-wynik[!is.na(wynik$dysleksja) & wynik$dysleksja == TRUE,]
wykresK <- ggHistMatury2(wynikK, 'dysleksja')

wynikM <- wynik[!is.na(wynik$dysleksja) & wynik$dysleksja == FALSE,]
wykresM <- ggHistMatury2(wynikM, 'niedysleksja')

wynikN <- wynik[is.na(wynik$dysleksja),]
wykresN <- ggHistMatury2(wynikN, 'NAdysleksja')

grid.arrange(wykresK, wykresM, wykresN, ncol=3)

ggHistMatury3<-function(data, tytul, filtr, zamienNA=NA){
  filtr <- filtr
  wynikZmod <- data
  if (is.na(zamienNA)) {
    wynikZmod <- wynikZmod[!is.na(wynikZmod[,filtr]),]
  }
  else{
    wynikZmod[is.na(wynikZmod[, filtr]), filtr] <- zmienNA # zamienia NA w kolumnie filtr na 'zmianNA'
  }
  pytania <- wynikZmod[,grep("^k_[0-9]*", names(wynikZmod))]
  sum_wynik <- rowSums(pytania, na.rm=T)
  wynikZmod$procent_wynik <- 100 * sum_wynik/max(sum_wynik, na.rm=T)
  krok <- 100 * 1/max(sum_wynik,  na.rm=T)
  p <- ggplot(wynikZmod, aes(x=procent_wynik, fill=wynikZmod[,filtr], y = ..density.. * 100), environment = environment()) +
    scale_fill_manual(values = c("red", "blue", "green", "black"), name = filtr) +
    geom_histogram(binwidth=krok, binwidth=.5, alpha=.3, position="identity") +
    xlab("% punktów") +
    ylab("% uczniów") +
    geom_vline(xintercept=30, color='red', linetype="longdash") +
    ggtitle(tytul) 
  return(p)
}

ggHistMatury3(wynik, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="plec")

ggHistMatury3(wynik, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="dysleksja")

# laureaci mają 100%
#ggHistMatury3(wynik, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="laureat")

wynikPoprawki <- wynik
wynikPoprawki$poprawkowa <- !is.na(wynikPoprawki$pop_podejscie)
table(wynikPoprawki$poprawkowa) # poprawiających do niepoprawiajacych ~ 1:10
# poprawiający tworzą wyraźnie odrębną populację i większość z nich nie zdaje
ggHistMatury3(wynikPoprawki, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="poprawkowa")

# bez poprawaijących rozkład jest bardziej "gaussowy" i  znika pik przed progiem 30%
wynikBezPoprawki <- wynik[is.na(wynik$pop_podejscie),]
ggHistMatury2(wynikBezPoprawki, paste("Wyniki matury", typ_matury, "w", rok, "roku"))
ggHistMatury2(wynik, paste("Wyniki matury", typ_matury, "w", rok, "roku"))
ggHistMatury3(wynikBezPoprawki, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="plec")

# wśród poprawiających dziewczyny radzą sobie trochę słabiej
wynikZPoprawki <- wynik[!is.na(wynik$pop_podejscie),]
ggHistMatury3(wynikZPoprawki, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="plec")

# roczniki
# wycinam dziwne przypadki z przed 1900 roku i po 2014 roku oraz osoby z NA zamiast rocznika
wynikRocznik <- wynik
wynikRocznik <- wynikRocznik[!wynikRocznik$rocznik>2014 & !wynikRocznik$rocznik<1900 & !is.na(wynikRocznik$rocznik),]
# wycinam też powtarzających maturę
wynikRocznik <- wynikRocznik[is.na(wynikRocznik$pop_podejscie),]

p <- ggplot(wynikRocznik, aes(x=rocznik, y=..density.. * 100), environment = environment()) +
  geom_histogram(binwidth=1) +
  xlab("rocznik") +
  ylab("% uczniów") +
  ggtitle("Roczniki zdające maturę z matematyki podstawową w 2014 roku") 
p

# grupuje roczniki:
# po pierwsze "we właściwym wieku", tzn max rozkładu roczników
najliczniejszy <- as.numeric(names(which.max(table(wynikRocznik$rocznik))))
wynikRocznik$grupaRocznikow[wynikRocznik$rocznik == najliczniejszy] <- najliczniejszy
wynikRocznik$grupaRocznikow[wynikRocznik$rocznik > najliczniejszy] <- paste(">", najliczniejszy)
wynikRocznik$grupaRocznikow[wynikRocznik$rocznik < najliczniejszy & wynikRocznik$rocznik >= (najliczniejszy - 5)] <- paste(najliczniejszy - 1, '-', najliczniejszy - 5)
wynikRocznik$grupaRocznikow[wynikRocznik$rocznik < najliczniejszy - 5] <-  paste("<", najliczniejszy -5)

ggHistMatury3(wynikRocznik, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="grupaRocznikow")




