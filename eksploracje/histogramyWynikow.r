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
  
  wynik$plec <- factor(wynik$plec, levels=c("k", "m"), labels=c("żeńska", "męska"))
  names(wynik)[names(wynik) == 'plec'] <- 'płeć'
  wynik$dysleksja <- factor(wynik$dysleksja, levels=c(FALSE, TRUE), labels=c("nie", "tak"))
  
  nazwyKolumn <- names(wynik)
  tytul <- paste("wyniki matury", typ_matury, "w", rok, "roku")
  
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
      scale_x_continuous(breaks = seq(0, 100, by = 10)) +
      ggtitle(info)
      #labs(title = (paste(tytul, '\n' ,info ))) +
      #theme(plot.title = element_text(hjust = 0.5))    
    return(p)
  }
  
  wszyscyHist <- ggHistMatury2(wynik, "wszyscy zdający")
  
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
      scale_x_continuous(breaks = seq(0, 100, by = 10)) +
      # geom_vline(xintercept=30, color='black', linetype="longdash") +
      ggtitle(info)
      #labs(title = (paste(tytul, '\n' ,info ))) +
      #theme(plot.title = element_text(hjust = 0.5))      
    return(p)
  }
  
  plecHist <- ggHistMatury3(wynik, "z podziałem na płeć", filtr="płeć")
  
  dysHist <- ggHistMatury3(wynik, "dyslektycy i niedyslektycy", filtr="dysleksja")
  
  # laureaci mają 100%, nieciekawe.
  #ggHistMatury3(wynik, filtr="laureat")
  
  # osoby poprawiające łączę w jedną grupę
  wynikPoprawki <- wynik
  wynikPoprawki$poprawkowa <- !is.na(wynikPoprawki$pop_podejscie)
  wynikPoprawki$poprawkowa <- factor(wynikPoprawki$poprawkowa, levels=c(FALSE, TRUE), labels=c("nie", "tak"))
  
  # table(wynikPoprawki$poprawkowa) # z matematyki podst. poprawiających do niepoprawiajacych ~ 1:10
  # z matematyki podst. poprawiający tworzą wyraźnie odrębną populację i większość z nich nie zdaje
  poprHist <- ggHistMatury3(wynikPoprawki, "po raz pierwszy i poprawiający", filtr="poprawkowa")
  
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
  
  grupyWiekowe <- c(paste(rok - najliczniejszy - 1, "i mniej"),
                    paste(rok - najliczniejszy),
                    paste(rok - najliczniejszy + 1, '-', rok - najliczniejszy + 5),
                    paste(rok - najliczniejszy + 6, "i więcej"))
  
  wynikRocznik$wiek <- factor(wynikRocznik$wiek, levels = grupyWiekowe)
  
  
  wiekHist <- ggHistMatury3(wynikRocznik, "wg rocznika (bez powtarzających)", filtr="wiek", kolory = c("green", "blue", "red", "black"))
  
  # grupy hist
  # wlozyc pozniej do funkcji
  # zrobione szybko i naiwnie, ale powinno dzialac
  
  kategoria <- c("płeć", "płeć",
                 "dysleksja", "dysleksja",
                 "poprawkowa", "poprawkowa",
                       "wiek", "wiek", "wiek", "wiek")
  grupa <- c("żeńska", "męska",
             "brak dysleksji", "dysleksja",
             "pierwsze podejście", "poprawkowa",
             grupyWiekowe[1], grupyWiekowe[2], grupyWiekowe[3], grupyWiekowe[4])
  liczba <- c(
    length(which(wynik$płeć == "żeńska")),
    length(which(wynik$płeć == "męska")),
    length(which(wynik$dysleksja == "nie")),
    length(which(wynik$dysleksja == "tak")),
    length(which(wynikPoprawki$poprawkowa == "nie")),
    length(which(wynikPoprawki$poprawkowa == "tak")),
    length(which(wynikRocznik$wiek == grupyWiekowe[1])),
    length(which(wynikRocznik$wiek == grupyWiekowe[2])),
    length(which(wynikRocznik$wiek == grupyWiekowe[3])),
    length(which(wynikRocznik$wiek == grupyWiekowe[4]))
  )
  
  liczba <- liczba/1000
  
  liczebnosc_grup <- data.frame(kategoria, grupa, liczba)
  liczebnosc_grup$grupa = factor(liczebnosc_grup$grupa, levels=rev(grupa))
  liczebnosc_grup$kategoria = factor(liczebnosc_grup$kategoria, levels=c("płeć", "dysleksja", "poprawkowa", "wiek"))
  
  # moze da sie prosciej...
  grupHist <- ggplot(liczebnosc_grup, aes(x=grupa, y=liczba, fill=kategoria)) +
    geom_bar(position=position_dodge(width=0.8), alpha=0.7, stat='identity') +
    xlab("") +
    ylab("liczba uczniów (w tysiącach)") +
    coord_flip() +
    ggtitle("kto zdaje?")
  
  # zebranie wykresów razem
  multiplots <- arrangeGrob(wszyscyHist, grupHist, plecHist, dysHist, poprHist, wiekHist,
                            ncol=2, nrow=3, main=tytul,
                            sub = textGrob("Piotr Migdał, Marta Czarnocka-Cieciura, https://github.com/stared/delab-matury",
                                           x = 0, hjust = -0.1, vjust=0.1,
                                           gp = gpar(fontsize = 9)))
  #multiplots <- do.call(arrangeGrob, list(wszyscyHist, plecHist, dysHist, poprHist, wiekHist, ncol = 1, nrow = 5))
  
  # utworzenie katalogu wyjściowego i zapisanie wykresów
  
  if (is.na(sciezkaOut)) {
    multiplots
  } else {
    dir.create(sciezkaOut, recursive=TRUE)
    plik_wyjsciowy <- paste(paste(gsub(" ", "_", typ_matury), rok, sep="_"), "png", sep=".")
    sciezkaOutFile <- paste(sciezkaOut, plik_wyjsciowy, sep='')
    ggsave(multiplots, file=sciezkaOutFile, width=10, height=8, dpi=150)
  }
  #return(c(wszyscyHist, plecHist, dysHist, poprHist, wiekHist))
}

## tak robimy pojedyczny histogram
histogramyF("fizyka podstawowa", 2014)

testy <- read.csv(paste(bazowaSciezka, "testy.csv", sep = ''),stringsAsFactors=F)
czesc <- unique(testy$czesc_egzaminu[testy$rodzaj_egzaminu == "matura"])
czesc <- czesc[!is.na(czesc)]

laczenie <- lapply(czesc, function(x){
  histogramyF(x, 2014, sciezkaOut="../owoce/histogramy/")
})

#multiplots <- do.call(arrangeGrob, c(as.list(histogramy2014), ncol=5, nrow=20))
#ggsave(multiplots, file="histogramy2014.pdf", width=8, height=300)
