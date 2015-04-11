# Pobiera dane o wynikach matur, łączy z danymi o uczniach (uczniowie i uczniowieTesty)
# zapisuje do utworzonego katalogu "wyniki" w postaci plików .csv.
# Również sciąga i zapisuje testy
# Jeśli pobrane wyniki już tam są, nie pobiera ponownie.
# Przed odpaleniem skryptu lepiej się upewnić, czy working directory jest właściwe!

library(ZPD)

dir.create("wyniki", showWarnings = FALSE) # jeśli katalog już jest - zostaje stary (z zawartością)

src <- polacz()
# uczniowieTesty pobierają się za długo
testy <- pobierz_testy(src)
testy <- testy %>% collect()
write.csv(testy, "wyniki/testy.csv")

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
  sciezka <- paste("wyniki/",file_name, sep = '')
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



# sciaga wszystkie matury (nie wiem, co zrobi, jeśli w którymś roku nie było danego typu matury)
x <- mapply(download_wyniki, powtCzesc, powtLata)
