library(ggplot2)
library(gridExtra)

# funkcja tworząca histogram z wyników pobranych z pliku 
# wyniki muszą być wcześniej pobrane i zapisane w bazowaSciezka 
# w formacie: typ_matury_rok.csv ("_" zamiast spacji w typie matury)
# np. przez skrypt sciaganieWynikow.r
# skrypt powinien być uruchamiany z folderu, w którym jest repozytorium, a nie sam skrypt.

bazowaSciezka <- "../dane/wyniki/"

histogramyF <- function (typ_matury, rok, sciezkaOut=NA) {
  
  # uwaga: czy na poczatku odfiltrować uczniów mających dziwne wartości zmiennych rocznik (NA | >2014 | <1900) i plec (NA)? Odfiltrować wiersze z samymi NA?
  # pobranie pliku
  file_name <- paste(paste(gsub(" ", "_", typ_matury), rok, sep="_"), "csv", sep=".")
  sciezka <- paste(bazowaSciezka, file_name, sep = '')
  print(paste("wczytuję dane z pliku ", sciezka, ", może chwilę potrawć", sep = ''))
  wynik <- read.csv(sciezka, stringsAsFactors=F)
  nazwyKolumn <- names(wynik)
  tytul <- paste("Wyniki matury", typ_matury, "w", rok, "roku")

  # histogram bez podziału na grupy
  ggHistMatury2<-function(wynik, info){
    pytania <- wynik[,grep("^k_[0-9]*", nazwyKolumn)]
    sum_wynik <- rowSums(pytania, na.rm=T)
    procent_wynik <- 100 * sum_wynik/max(sum_wynik, na.rm=T)
    krok <- 100 * 1/max(sum_wynik,  na.rm=T)
    p <- ggplot(data.frame(procent_wynik), aes(x=procent_wynik, y = ..density.. * 100)) +
      geom_histogram(colour="white", binwidth=krok) +
      xlab("% punktów") +
      ylab("% uczniów") +
      ggtitle(info)
      #labs(title = (paste(tytul, '\n' ,info ))) +
      #theme(plot.title = element_text(hjust = 0.5))    
    return(p)
  }
  
  wszyscyHist <- ggHistMatury2(wynik, "Wszyscy zdający")
  
  # kilka histogramów na jednym wykresie (dla różnych wartości zmiennej filtr)
  # wszystkie wyskalowane do 100% (utrata informacji o liczebności poszczególnych grup)
  ggHistMatury3<-function(data, info, filtr, zamienNA=NA, kolory=c("red", "blue")){
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
    p <- ggplot(wynikZmod, aes(x=procent_wynik, fill=wynikZmod[,filtr], y=..density.. * 100), environment=environment()) +
      scale_fill_manual(values=kolory, name=filtr) +
      geom_histogram(binwidth=krok, binwidth=.5, alpha=.3, position="identity") +
      xlab("% punktów") +
      ylab("% uczniów") +
      geom_vline(xintercept=30, color='red', linetype="longdash") +
      ggtitle(info)
      #labs(title = (paste(tytul, '\n' ,info ))) +
      #theme(plot.title = element_text(hjust = 0.5))      
    return(p)
  }
  
  plecHist <- ggHistMatury3(wynik, "Z podziałem na płeć", filtr="plec")
  
  dysHist <- ggHistMatury3(wynik, "Dyslektycy i niedyslektycy", filtr="dysleksja")
  
  # laureaci mają 100%, nieciekawe.
  #ggHistMatury3(wynik, filtr="laureat")
  
  # osoby poprawiające łączę w jedną grupę
  wynikPoprawki <- wynik
  wynikPoprawki$poprawkowa <- !is.na(wynikPoprawki$pop_podejscie)
  # table(wynikPoprawki$poprawkowa) # z matematyki podst. poprawiających do niepoprawiajacych ~ 1:10
  # z matematyki podst. poprawiający tworzą wyraźnie odrębną populację i większość z nich nie zdaje
  poprHist <- ggHistMatury3(wynikPoprawki, "Zdający po raz pierwszy i poprawiający", filtr="poprawkowa")
  
  # dla matematyki podst. bez poprawaijących rozkład jest bardziej "gaussowy" i  znika pik przed progiem 30%
  # wynikBezPoprawki <- wynik[is.na(wynik$pop_podejscie),]
  # ggHistMatury2(wynikBezPoprawki, "Zdający po raz pierwszy")
  # ggHistMatury2(wynik, "wszyscy zdający")
  # ggHistMatury3(wynikBezPoprawki, "Zdający po raz pierwszy", filtr="plec")
  
  # z matematyki wśród poprawiających dziewczyny radzą sobie trochę słabiej
  # wynikZPoprawki <- wynik[!is.na(wynik$pop_podejscie),]
  # ggHistMatury3(wynikZPoprawki, "Poprawiający", filtr="plec")
  
  # roczniki
  wynikRocznik <- wynik
  # wycinam dziwne przypadki z przed 1900 roku i po 2014 roku oraz osoby z NA zamiast rocznika
  wynikRocznik <- wynikRocznik[!wynikRocznik$rocznik>2014 & !wynikRocznik$rocznik<1900 & !is.na(wynikRocznik$rocznik),]
  # wycinam też powtarzających maturę
  wynikRocznik <- wynikRocznik[is.na(wynikRocznik$pop_podejscie),]
  
  # histogram roczników
  # p <- ggplot(wynikRocznik, aes(x=rocznik, y=..density.. * 100), environment = environment()) +
  #   geom_histogram(binwidth=1) +
  #   xlab("rocznik") +
  #   ylab("% uczniów") +
  #   ggtitle("Roczniki zdające maturę z matematyki podstawową w 2014 roku") 
  # p
  
  # grupuje roczniki:
  # po pierwsze "we właściwym wieku", tzn max rozkładu roczników
  najliczniejszy <- as.numeric(names(which.max(table(wynikRocznik$rocznik))))
  wynikRocznik$wiek[wynikRocznik$rocznik == najliczniejszy] <- rok - najliczniejszy
  wynikRocznik$wiek[wynikRocznik$rocznik > najliczniejszy] <- paste(rok - najliczniejszy - 1, "i mniej")
  wynikRocznik$wiek[wynikRocznik$rocznik < najliczniejszy & wynikRocznik$rocznik >= (najliczniejszy - 5)] <- paste(rok - najliczniejszy + 1, '-', rok - najliczniejszy + 5)
  wynikRocznik$wiek[wynikRocznik$rocznik < najliczniejszy - 5] <-  paste(rok - najliczniejszy + 6, "i więcej")
  
  # posortowanie rocznikow
  wynikRocznik$wiek <- factor(wynikRocznik$wiek, levels = c(paste(rok - najliczniejszy - 1, "i mniej"),
                                                            paste(rok - najliczniejszy),
                                                            paste(rok - najliczniejszy + 1, '-', rok - najliczniejszy + 5),
                                                            paste(rok - najliczniejszy + 6, "i więcej")))
  
  
  wiekHist <- ggHistMatury3(wynikRocznik, "Zdający wg wieku\n(na podstawie rocznika, bez powtarzających)", filtr="wiek", kolory = c("green", "blue", "red", "black"))
  
  # zebranie wykresów razem
  multiplots <- arrangeGrob(wszyscyHist, plecHist, dysHist, poprHist, wiekHist, ncol=2, nrow=3, main=tytul)
  #multiplots <- do.call(arrangeGrob, list(wszyscyHist, plecHist, dysHist, poprHist, wiekHist, ncol = 1, nrow = 5))
  
  # utworzenie katalogu wyjściowego i zapisanie wykresów
  
  if (is.na(sciezkaOut)) {
    multiplots
  } else {
    dir.create(sciezkaOut, recursive=TRUE)
    plik_wyjsciowy <- paste(paste(gsub(" ", "_", typ_matury), rok, sep="_"), "png", sep=".")
    sciezkaOutFile <- paste(sciezkaOut, plik_wyjsciowy, sep='')
    ggsave(multiplots, file=sciezkaOutFile, width=10, height=8)
  }
  #return(c(wszyscyHist, plecHist, dysHist, poprHist, wiekHist))
}

## tak robimy pojedyczny histogram
# histogramyF("fizyka_podstawowa", 2014)

testy <- read.csv(paste(bazowaSciezka, "testy.csv", sep = ''),stringsAsFactors=F)
czesc <- unique(testy$czesc_egzaminu[testy$rodzaj_egzaminu == "matura"])
czesc <- czesc[!is.na(czesc)]

laczenie <- lapply(czesc, function(x){
  histogramyF(x, 2014, sciezkaOut="owoce/histogramy")
})

#multiplots <- do.call(arrangeGrob, c(as.list(histogramy2014), ncol=5, nrow=20))
#ggsave(multiplots, file="histogramy2014.pdf", width=8, height=300)
