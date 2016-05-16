# Pobiera dane o wynikach matur, łączy z danymi o uczniach (uczniowie i uczniowieTesty)
# zapisuje do utworzonego katalogu "wyniki" w postaci plików .csv.
# Również sciąga i zapisuje testy
# Jeśli pobrane wyniki już tam są, nie pobiera ponownie.
# skrypt powinien być uruchamiany z folderu, w którym jest repozytorium, a nie sam skrypt.

# devtools::install_github('zozlak/ZPD')
library(ZPD)

bazowaSciezka <- "dane/wyniki/"
dir.create(bazowaSciezka, recursive=TRUE) # jeśli katalog już jest - zostaje stary (z zawartością)

src <- polacz()
# uczniowieTesty pobierają się za długo
testy <- pobierz_testy(src)
testy <- testy %>% collect()
write.csv(testy, paste(bazowaSciezka, "testy.csv", sep = ''))

#części matur i lata:
czesc <- unique(testy$czesc_egzaminu[testy$rodzaj_egzaminu == "matura"])
czesc <- czesc[!is.na(czesc)]

lata <- unique(testy$rok[testy$rodzaj_egzaminu == "matura"])
lata <- lata[!is.na(lata)]

# zestaw parametrow do funkcji download_wyniki
powtCzesc <- rep(czesc, length(lata))
powtLata <- sort(rep(lata, length(czesc)))

# funkcja pobierająca wyniki konkretnej matury
download_wyniki <- function(typ_matury, rok){
  file_name <- paste(paste(gsub(" ", "_", typ_matury), rok, sep="_"), "csv", sep=".")
  sciezka <- paste(bazowaSciezka, file_name, sep = '')
  if (file.exists(sciezka)) {
    print(paste("plik ", file_name, " już istnieje, przechodzę do kolejnego"))
    rm(file_name, sciezka)
  }
  else {
    print(paste("pobieranie ", typ_matury, " ", rok))
    src <- polacz()
    uczniowie <- pobierz_uczniow(src)
    uczniowieTesty <- pobierz_dane_uczniowie_testy(src)
    wyniki <- pobierz_wyniki_egzaminu(src, "matura", typ_matury, rok, T)
    zlaczone <- left_join(wyniki, uczniowieTesty)
    zlaczone <- left_join(zlaczone, uczniowie)
    zlaczone <- zlaczone %>% collect() # pobranie wyników z bazy danych na komputer
    write.csv(zlaczone, sciezka)
    rm(wyniki, file_name, sciezka)
  }
}

# sciaga jedno; powinno być krótkie
download_wyniki("informatyka rozszerzona", 2015)

# tu niestety obecnie urywa polaczenie
download_wyniki("j. polski podstawowa", 2015)

# sciaga wszystkie matury (nie wiem, co zrobi, jeśli w którymś roku nie było danego typu matury)
x <- mapply(download_wyniki, powtCzesc, powtLata)

# ostatni rok bez duzych
x <- mapply(download_wyniki, czesc[-c(1, 2, 19)], 2015)
