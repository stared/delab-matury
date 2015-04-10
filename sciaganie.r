# Pobiera danych o wynikach matur i zapisuje do utworzonego katalogu "wyniki" w postaci plików .csv.
# Również sciąga i zapisuje testy
# Jeśli pobrane wyniki już tam są, pobiera ponownie.
# Przed odpaleniem skryptu lepiej się upewnić, czy working directory jest właściwe.

library(ZPD)
library(ggplot2)

dir.create("wyniki", showWarnings = FALSE)
setwd("wyniki")

src <- polacz()
testy <- pobierz_testy(src)
testy <- testy %>% collect()
write.csv(testy, "testy.csv")

#rodzaje matur i lata:
rodzaj <-c("matura", "matura poprawkowa")
czesc <- unique(testy$czesc_egzaminu[testy$rodzaj_egzaminu %in% rodzaj])
czesc <- czesc[!is.na(czesc)]

lata <- unique(testy$rok[testy$rodzaj_egzaminu %in% rodzaj])
lata <- lata[!is.na(lata)]

powtCzesc <- rep(czesc, length(lata))
powtLata <- sort(rep(lata, length(czesc)))

# funkcja pobierająca wyniki konkretnej matury
download_wyniki <- function(typ_matury, rok){
  print(paste("pobieranie ", typ_matury, " ", rok))
  src = polacz()
  wyniki <- pobierz_wyniki_egzaminu(src, "matura", typ_matury, rok, T)
  wyniki <- wyniki %>% collect() # pobranie wyników z bazy danych na komputer
  file_name <- paste(paste(gsub(" ", "_", typ_matury), rok, sep="_"), "csv", sep=".")
  write.csv(wyniki, file_name)
  rm(wyniki, file_name)
}

mapply(download_wyniki, czesci, lata)

# uczniowieTesty pobierają się za długo

setwd("../")
