library(dplyr)

przetworz <- function(rok){
  matury <- read.csv("../dane/wyniki/testy.csv") %>%
    filter(rodzaj_egzaminu=="matura", czy_egzamin==TRUE, rok==rok) %>%
    select(czesc_egzaminu) %>%
    unique
  
  matury$czesc_egzaminu %>%
    paste0("../dane/wyniki/", ., " ", rok, ".csv") %>%
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
  przedmioty <- read.csv(paste0("../dane/wyniki/j._polski_podstawowa_", rok, ".csv"))
  
  # wyrzucamy poprawkowe
  przedmioty <- przedmioty %>% filter(is.na(pop_podejscie))
  
  rownames(przedmioty) <- przedmioty$id_obserwacji
  przedmioty <- select(przedmioty, id_szkoly, plec, rocznik, dysleksja)
  
  for (i in 1:nrow(matury)) {
    
    row <- matury[i,]
    print(row$sciezka)
    
    matura <- read.csv(row$sciezka) %>% filter(is.na(pop_podejscie))
    
    rownames(matura) <- matura$id_obserwacji
    
#     przedmioty[rownames(matura), paste0(row$przedmiot, "_laureat")] <- matura$laureat
    przedmioty[rownames(matura), row$nazwa] <- matura %>%
      select(starts_with("k_")) %>% rowSums(na.rm=T)
  }
  
  dir.create("../dane/przetworzone/", recursive=TRUE)
  write.csv(przedmioty, paste0("../dane/przetworzone/wyniki_przetworzone_", rok, ".csv"))
  
  # dane bez poprawiajacych
}

lapply(2010:2014, przetworz)