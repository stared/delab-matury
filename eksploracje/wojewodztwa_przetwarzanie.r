library(maptools)
library(rgdal)
library(ggplot2)
library(dplyr)
library(scales)

wojewodztwa <- readOGR(dsn="../dane/PRG_jednostki_administracyjne_v8/wojew¢dztwa.shp",
                       layer="wojew¢dztwa")

woj_df <- fortify(wojewodztwa, region = "jpt_nazwa_")
woj_df$id = iconv(woj_df$id, "windows-1250", "UTF-8") # polskie znaki
woj_df$group = iconv(woj_df$group, "windows-1250", "UTF-8")

centroidy <- as.data.frame(coordinates(wojewodztwa))
names(centroidy) <- c("Longitude", "Latitude")
nazwy_woj <- iconv(wojewodztwa@data$jpt_nazwa_, "windows-1250", "UTF-8") # polskie znaki

centroidy_df <- data.frame(id = nazwy_woj, centroidy)

# matury
zapisz <- function(rok) {
  # wczytanie danych 
  #rok <- 2010
  szkoly <- read.csv(paste0("../dane/przetworzone/szkoly", rok, ".csv"))
  wyniki <- read.csv(paste0("../dane/przetworzone/wyniki_przetworzone_", rok, ".csv"))
  
  dane <- merge(wyniki, szkoly, by = "id_szkoly")
  
  matury <- colnames(dane)[c(grep(".*podstawowa",colnames(dane)), grep(".*rozszerzona",colnames(dane)))]
  
  # średnie wyniki w PL:
  srPL <- sapply(matury, function(nazwa){
    print(nazwa)
    max <- max(dane[,colnames(dane)==nazwa], na.rm=T)
    #print(max)
    mean(dane[,colnames(dane)==nazwa]/max*100, na.rm=T)
  })
  
  # udział zdających w PL:
  zdajPL <- sapply(matury, function(nazwa){
    print(nazwa)
    matura <- dane[,colnames(dane)==nazwa]
    return(100*sum(!is.na(matura))/length(matura))
  })
  
  # max punktów
  maxPunkty <- sapply(matury, function(nazwa){
    max(dane[,colnames(dane)==nazwa], na.rm=T)
  })
  
  # średnie wyniki dla województw
  sapply(matury, function(nazwa){
    print(nazwa)
    max <- max(dane[,colnames(dane)==nazwa], na.rm=T)
    #print(max)
    tapply(dane[,colnames(dane)==nazwa]/max*100, dane$wojewodztwo_szkoly, mean, na.rm=T)
  }) %>% data.frame(.) -> srWoj
  
  colnames(srWoj) <- paste0("sr_", colnames(srWoj))
  srWoj$id<-rownames(srWoj)
  
  # połączenie ze współrzędnymi województw
  # woj_wyniki <- merge(woj_df, srWoj, by="id", sort = FALSE, all.x=TRUE)
  
  # % zdających w wojewodztwach
  sapply(matury, function(nazwa){
    print(nazwa)
    tapply(dane[,colnames(dane)==nazwa], dane$wojewodztwo_szkoly,
           function(x){return(100*sum(!is.na(x))/length(x))})
  }) %>% data.frame(.) -> zdajWoj
  
  colnames(zdajWoj) <- paste0("zdaj_", colnames(zdajWoj))
  zdajWoj$id<-rownames(zdajWoj)
  
  # liczba zdających w wojewodztwach
  sapply(matury, function(nazwa){
    print(nazwa)
    tapply(dane[,colnames(dane)==nazwa], dane$wojewodztwo_szkoly,
           function(x){return(sum(!is.na(x)))})
  }) %>% data.frame(.) -> liczWoj
  
  liczWoj$id<-rownames(liczWoj)
  
  # liczba maturzystów w województwach:
  tapply(dane$j_polski_podstawowa, dane$wojewodztwo_szkoly,
         function(x){return(length(x))}) %>% data.frame(.) -> maturzWoj
  colnames(maturzWoj)<-"maturzysci"
  maturzWoj$id<-rownames(maturzWoj)
  
  
  # połączenie ze współrzędnymi centroidów wojewodztw
  dane_woj <- merge(centroidy_df, zdajWoj, by="id")
  dane_woj <- merge(dane_woj, srWoj, by="id")
  dane_woj <- merge(dane_woj, maturzWoj, by="id")
  dane_woj$rok <- rep(rok, nrow(dane_woj))
  colnames(dane_woj)[colnames(dane_woj=="id")] <- "wojewodztwa"
  #write.csv(dane_woj, paste0("../mapyJs/wojewodztwa_", rok, ".csv"))
  return(dane_woj)
}

dane_komplet <- lapply(2010:2014, zapisz)
dane_komplet2 <- rbind(dane_komplet[[1]], dane_komplet[[2]], dane_komplet[[3]], dane_komplet[[4]])
write.csv(dane_komplet2, "../mapyJs/wojewodztwa.csv")
