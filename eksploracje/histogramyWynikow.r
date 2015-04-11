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

ggHistMatury3<-function(wynik, tytul, filtr, zamienNA=NA){
  wynikZmod <- wynik
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
  p <- ggplot(data.frame(wynikZmod), aes(x=procent_wynik, fill=get(filtr), y = ..density..*100)) +
    scale_fill_manual(values = c("red","blue", "green"), name = filtr)+
    geom_histogram(binwidth=krok, binwidth=.5, alpha=.3, position="identity") +
    xlab("% punktów") +
    ylab("% uczniów") +
    ggtitle(tytul) 
  return(p)
}

ggHistMatury3(wynik, tytul=paste("Wyniki matury", typ_matury, "w", rok, "roku"), filtr="plec")
