library(dplyr)
library(ggplot2)

matury <- read.csv("../dane/wyniki/testy.csv") %>%
  filter(rodzaj_egzaminu=="matura", czy_egzamin==TRUE, rok==2014)

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
rownames(przedmioty) <- przedmioty$id_obserwacji
przedmioty <- select(przedmioty, id_szkoly, plec, rocznik, dysleksja)

# z jakiegos powodu kilkakrotnie robi te same przedmioty
# no i cos nie wrzuca...
apply(matury, 1, function (row) {
  print(row["sciezka"])
  
  matura <- read.csv(row["sciezka"])
  rownames(matura) <- matura$id_obserwacji

  przedmioty[rownames(matura), paste0(row["przedmiot"], "_laureat")] <- matura$laureat
  przedmioty[rownames(matura), row["nazwa"]] <- matura %>%
    select(starts_with("k_")) %>% rowSums(na.rm=T)
  
})



