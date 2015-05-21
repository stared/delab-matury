library(dplyr)

matury <- read.csv("../dane/wyniki/testy.csv") %>%
  filter(rodzaj_egzaminu=="matura", czy_egzamin==TRUE, rok==2014) %>%
  select(czesc_egzaminu) %>%
  unique

szkoly <- read.csv("../dane/szkoly2014.csv") %>%
  select(c(id_szkoly, rodzaj_gminy, typ_szkoly, publiczna, wielkosc_miejscowosci))

matury$czesc_egzaminu %>%
  paste0("../dane/wyniki/", ., " 2014.csv") %>%
  gsub(" ", "_", .) ->
  matury$sciezka

matury$czesc_egzaminu %>%
  gsub("\\.", "", .) %>%
  gsub(" ", "_", .) ->
  matury$nazwa

matury$nazwa %>%
  gsub("_podstawowa", "", .) %>%
  gsub("_rozszerzona", "", .) ->
  matury$przedmiot

# indeksy, informacje ogolne
przedmioty <- read.csv("../dane/wyniki/j._polski_podstawowa_2014.csv")
# trzeba wyrzucac poprawkowe
przedmioty[1000:1010,"pop_podejscie"]
rownames(przedmioty) <- przedmioty$id_obserwacji
przedmioty <- select(przedmioty, id_szkoly, plec, rocznik, dysleksja)

# to odziala!
for (i in 1:nrow(matury)) {

  row <- matury[i,]
  print(row$sciezka)
  
  matura <- read.csv(row$sciezka)
  # trzeba wyrzucac poprawkowe
  rownames(matura) <- matura$id_obserwacji
  
  #przedmioty[rownames(matura), paste0(row$przedmiot, "_laureat")] <- matura$laureat
  przedmioty[rownames(matura), row$nazwa] <- matura %>%
    select(starts_with("k_")) %>% rowSums(na.rm=T)
}

#dir.create("../dane/przetworzone/", recursive=TRUE)

przedmioty$plec <- factor(przedmioty$plec, levels=c("k", "m"), labels=c("kobiety", "mężczyźni"))
names(przedmioty)[names(przedmioty) == 'plec'] <- 'płeć'
przedmioty$dysleksja <- factor(przedmioty$dysleksja, levels=c(FALSE, TRUE), labels=c("brak dysleksji", "dysleksja"))

# przetwarzanie danych o wieku
rok <- 2014 # do zmiany, jesli beda dane z roznych lat
# wycinam dziwne przypadki z przed 1900 roku i po 2014 roku oraz osoby z NA zamiast rocznika
przedmioty <- przedmioty[!przedmioty$rocznik>2014 & !przedmioty$rocznik<1900 & !is.na(przedmioty$rocznik),]
najliczniejszy <- as.numeric(names(which.max(table(przedmioty$rocznik))))
przedmioty$wiek[przedmioty$rocznik == najliczniejszy] <- rok - najliczniejszy
przedmioty$wiek[przedmioty$rocznik > najliczniejszy] <- paste(rok - najliczniejszy - 1, "i mniej")
przedmioty$wiek[przedmioty$rocznik < najliczniejszy & przedmioty$rocznik >= (najliczniejszy - 1)] <- paste(rok - najliczniejszy + 1)
przedmioty$wiek[przedmioty$rocznik < najliczniejszy - 1] <-  paste(rok - najliczniejszy + 2, "i więcej")

grupyWiekowe <- c(paste(rok - najliczniejszy - 1, "i mniej"),
                  paste(rok - najliczniejszy),
                  paste(rok - najliczniejszy + 1),
                  paste(rok - najliczniejszy + 2, "i więcej"))
przedmioty$wiek <- factor(przedmioty$wiek, levels = grupyWiekowe)

dane <- merge(przedmioty, szkoly, by="id_szkoly")

# TODO:
# uporządkować dane o szkolach
levels(dane$rodzaj_gminy)[levels(dane$rodzaj_gminy) == "dzielnica m.st. Warszawy"] <- "miejska"

dane$typ_szkoly[!(dane$typ_szkoly %in% c('LO', 'LP', 'T'))] <- NA
levels(dane$typ_szkoly)[levels(dane$typ_szkoly) == "LO"] <- "liceum ogólnokształcące"
levels(dane$typ_szkoly)[levels(dane$typ_szkoly) == "LP" | levels(dane$typ_szkoly) == "T"] <- "liceum profilowane lub technikum"


dane$publiczna[dane$publiczna] <- "publiczna"
dane$publiczna[dane$publiczna ==F] <- "prywatna"

wm <- dane$wielkosc_miejscowosci
dane$wielkosc_miejscowosci[wm<5000] <- "poniżej 5 tys."
dane$wielkosc_miejscowosci[wm>=5000 & wm<50000] <- "5 tys. - 50 tys."
dane$wielkosc_miejscowosci[wm>50000] <- "ponad 50 tys."

write.csv(dane, "histogramy/wyniki.csv")



