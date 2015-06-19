library(maptools)
library(rgdal)
library(ggplot2)
library(dplyr)
library(scales)

# TODO: 
# w legendzie: bd.
# % zdających
# zapisać mapy do pliku
# wojewodztwa: koła w środoku 


## kontury powiatów
powiaty <- readOGR(dsn="../dane/PRG_jednostki_administracyjne_v8/powiaty.shp", layer="powiaty")

powiaty_df <- fortify(powiaty, region = "jpt_nazwa_")

# czy podzbiór wystarczy?
podzbior <- seq(from=1, to=nrow(powiaty_df), by=2)
powiaty_df<-powiaty_df[podzbior,]

powiaty_df$id = iconv(powiaty_df$id, "windows-1250", "UTF-8")
powiaty_df$id = gsub("powiat ", "", powiaty_df$id)
powiaty_df$group = iconv(powiaty_df$group, "windows-1250", "UTF-8")
powiaty_df$group = gsub("powiat ", "", powiaty_df$group)

# poprawianie powiatów o takich samych nazwach:
# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="bielski"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="bielski" & powiaty_df$lat>4e+5] <- "bielski podlaskie"
powiaty_df$id[powiaty_df$id=="bielski" & powiaty_df$lat<4e+5] <- "bielski śląskie"


# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="brzeski"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="brzeski" & powiaty_df$long>5e+5] <- "brzeski małopolskie"
powiaty_df$id[powiaty_df$id=="brzeski" & powiaty_df$long<5e+5] <- "brzeski opolskie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="grodziski"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="grodziski" & powiaty_df$long>5e+5] <- "grodziski mazowieckie"
powiaty_df$id[powiaty_df$id=="grodziski" & powiaty_df$long<5e+5] <- "grodziski wielkopolskie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="krośnieński"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="krośnieński" & powiaty_df$long>5e+5] <- "krośnieński podkarpackie"
powiaty_df$id[powiaty_df$id=="krośnieński" & powiaty_df$long<5e+5] <- "krośnieński lubuskie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="nowodworski"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="nowodworski" & powiaty_df$lat>6e+5] <- "nowodworski pomorskie"
powiaty_df$id[powiaty_df$id=="nowodworski" & powiaty_df$lat<6e+5] <- "nowodworski mazowieckie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="opolski"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="opolski" & powiaty_df$long>5e+5] <- "opolski lubelskie"
powiaty_df$id[powiaty_df$id=="opolski" & powiaty_df$long<5e+5] <- "opolski opolskie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="ostrowski"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="ostrowski" & powiaty_df$long>5e+5] <- "ostrowski mazowieckie"
powiaty_df$id[powiaty_df$id=="ostrowski" & powiaty_df$long<5e+5] <- "ostrowski wielkopolskie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="średzki"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="średzki" & powiaty_df$lat>4e+5] <- "średzki wielkopolskie"
powiaty_df$id[powiaty_df$id=="średzki" & powiaty_df$lat<4e+5] <- "średzki dolnośląskie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="świdnicki"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="świdnicki" & powiaty_df$long>5e+5] <- "świdnicki lubelskie"
powiaty_df$id[powiaty_df$id=="świdnicki" & powiaty_df$long<5e+5] <- "świdnicki dolnośląskie"

# wybrane <- powiaty_df
# wybrane$fill <- wybrane$id=="tomaszowski"
# qplot(long, lat, data=wybrane, group=group , fill=fill, geom="polygon")
powiaty_df$id[powiaty_df$id=="tomaszowski" & powiaty_df$long>7e+5] <- "tomaszowski lubelskie"
powiaty_df$id[powiaty_df$id=="tomaszowski" & powiaty_df$long<7e+5] <- "tomaszowski łódzkie"

# województwa
wojewodztwa <- readOGR(dsn="../dane/PRG_jednostki_administracyjne_v8/wojew¢dztwa.shp",
                       layer="wojew¢dztwa")

woj_df <- fortify(wojewodztwa, region = "jpt_nazwa_")
woj_df$id = iconv(woj_df$id, "windows-1250", "UTF-8") # polskie znaki
woj_df$group = iconv(woj_df$group, "windows-1250", "UTF-8")

centroidy <- as.data.frame(coordinates(wojewodztwa))
names(centroidy) <- c("Longitude", "Latitude")
nazwy_woj <- iconv(wojewodztwa@data$jpt_nazwa_, "windows-1250", "UTF-8") # polskie znaki

centroidy_df <- data.frame(id = nazwy_woj, centroidy)

# wyniki matur z podziałem na wojewodztwa i na powiaty
rysujMapy(rok){
  szkoly <- read.csv(paste0("../dane/szkoly", rok, ".csv"))
  wyniki <- read.csv("../dane/przetworzone/sumy_laureaty.csv")
  
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
  
  srWoj$id<-rownames(srWoj)
  
  # połączenie ze współrzędnymi województw
  woj_wyniki <- merge(woj_df, srWoj, by="id", sort = FALSE, all.x=TRUE)
  
  # połączenie ze współrzędnymi centroidów wojewodztw
  woj_cent_wyniki <- merge(centroidy_df, srWoj, by="id")
  
  # % zdających w wojewodztwach
  sapply(matury, function(nazwa){
    print(nazwa)
    tapply(dane[,colnames(dane)==nazwa], dane$wojewodztwo_szkoly,
           function(x){return(100*sum(!is.na(x))/length(x))})
  }) %>% data.frame(.) -> zdajWoj
  
  zdajWoj$id<-rownames(zdajWoj)
  
  # połączenie ze współrzędnymi województw
  woj_zdaj <- merge(woj_df, zdajWoj, by="id", sort = FALSE, all.x=TRUE)
  
  # połączenie ze współrzędnymi centroidów wojewodztw
  woj_cent_zdaj <- merge(centroidy_df, zdajWoj, by="id")
  
  # średnie wyniki dla powiatow
  dane$powiatWojewodztwo <- paste(as.character(dane$powiat_szkoly),
                                  as.character(dane$wojewodztwo_szkoly))
  sapply(matury, function(nazwa){
    print(nazwa)
    max <- max(dane[,colnames(dane)==nazwa], na.rm=T)
    #print(max)
    tapply(dane[,colnames(dane)==nazwa]/max*100, dane$powiatWojewodztwo, mean, na.rm=T)
  }) %>% data.frame(.) -> srPow
  
  nazwy_woj <- unique(dane$wojewodztwo_szkoly)
  nazwy_pow <- rownames(srPow)
  nazwy_pow2 <- rownames(srPow)
  for (w in nazwy_woj){
    nazwy_pow <- gsub(paste(" ", w, sep=""), "", nazwy_pow)
  }
  pojedyncze <- names(table(nazwy_pow))[table(nazwy_pow)==1]
  podwojne <- names(table(nazwy_pow))[table(nazwy_pow)>1]
  for (i in 1:length(nazwy_pow)){
    p <- nazwy_pow[i]
    if (p %in% pojedyncze){
      nazwy_pow2[i] <- p
    }
  }  
  
  rownames(srPow) <- nazwy_pow2
  srPow <- srPow[rownames(srPow)!="NA",]
  
  srPow$id <- rownames(srPow)
  
  # połączenie ze współrzędnymi powiatów
  pow_wyniki <- merge(powiaty_df, srPow, by="id", sort = FALSE, all.x=TRUE)
  order <- order(pow_wyniki$order)
  pow_wyniki <- pow_wyniki[order,]
  
  # % zdających w powiatach
  sapply(matury, function(nazwa){
    print(nazwa)
    tapply(dane[,colnames(dane)==nazwa], dane$powiatWojewodztwo,
           function(x){return(100*sum(!is.na(x))/length(x))})
  }) %>% data.frame(.) -> zdajPow
  
  rownames(zdajPow) <- nazwy_pow2
  srPow <- zdajPow[rownames(zdajPow)!="NA",]
  
  zdajPow$id <- rownames(zdajPow)
  
  # połączenie ze współrzędnymi powiatów
  pow_zdaj <- merge(powiaty_df, zdajPow, by="id", sort = FALSE, all.x=TRUE)
  order <- order(pow_zdaj$order)
  pow_zdaj <- pow_zdaj[order,]
  
  # wykresy
  
  # własności graficzne wykresu
  theme_opts <- list(theme(panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           panel.background = element_blank(),
                           plot.background = element_rect(fill="#e6e8ed"),
                           panel.border = element_blank(),
                           axis.line = element_blank(),
                           axis.text.x = element_blank(),
                           axis.text.y = element_blank(),
                           axis.ticks = element_blank(),
                           axis.title.x = element_blank(),
                           axis.title.y = element_blank(),
                           plot.title = element_text(size=22)))
  
  # rysowanie mapy ze średnimi z wybranego przedmiotu w województwach:
  
  
  rysuj_woj <- function(matura, co){
    if(co=="wyniki"){
      tytul <- paste0("Średnie wyniki w ", rok, ": ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
      dane_woj <- woj_wyniki
      midpoint <- srPL[matura] 
    }
    if(co=="zdający"){
      tytul <- paste0("Procent zdających w ", rok, ": ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
      dane_woj <- woj_zdaj
      midpoint <- zdajPL[matura]
    }
    colnames(dane_woj)[colnames(dane_woj)==matura] <- "wyniki"
    print(paste(co, matura, rok))
    map <- ggplot(data = dane_woj, aes(long,lat, group=group, fill=wyniki)) + 
      geom_text(aes(label = id, x = Longitude, y = Latitude))
      geom_polygon(colour="black") +
      labs(title=tytul) + 
      coord_equal() + 
      scale_fill_gradient2(name=paste(co,"(%)"), low=muted("red"), high=muted("blue"), midpoint=midpoint) +
      scale_colour_manual(name = 'Brak danych', 
                          values =c("green", FALSE), labels = c('NA', "buu")) +
      theme_opts
    #ggsave(filename=paste0("../owoce/mapy/woj", "_", co, "_", matura, "_", rok, ".png"), plot=map)
    return(map)
  }
  
  rysuj_woj_kola <- function(matura, co){
    if(co=="wyniki"){
      tytul <- paste0("Średnie wyniki w ", rok, ": ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
      dane_woj <- woj_cent_wyniki
      midpoint <- srPL[matura] 
    }
    if(co=="zdający"){
      tytul <- paste0("Procent zdających w ", rok, ": ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
      dane_woj <- woj_cent_zdaj
      midpoint <- zdajPL[matura]
    }
    colnames(dane_woj)[colnames(dane_woj)==matura] <- "wyniki"
    print(paste(co, matura, rok))
    map <-   ggplot(dane_woj, aes(map_id = id)) + #"id" is col in your df, not in the map object 
      geom_map(colour= "gray", map = woj_df) +
      geom_point(data=dane_woj, 
                 aes(x=Longitude, y=Latitude, size=wyniki), 
                 fill="orange",pch=21) +
      expand_limits(x = woj_df$long, y =woj_df$lat) +
      labs(title=tytul) + 
      coord_equal() + 
      theme_opts 
    #ggsave(filename=paste0("../owoce/mapy/woj", "_", co, "_", matura, "_", rok, ".png"), plot=map)
    return(map)
  }
  

  
  rysuj_woj("biologia_podstawowa", "zdający")
  mapply(rysuj_woj, matura=c(matury,matury),
         co=c(rep("wyniki", length(matury)),rep("zdający", length(matury))))
  
  # rysowanie mapy ze średnimi wynikami w powiatach
  rysuj_pow <- function(matura, co){
    if(co=="wyniki"){
      tytul <- paste0("Średnie wyniki w ", rok, ": ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
      dane_pow <- pow_wyniki
      midpoint <- srPL[matura] 
    }
    if(co=="zdający"){
      tytul <- paste0("Procent zdających w ", rok, ": ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
      dane_pow <- pow_zdaj
      midpoint <- zdajPL[matura]
    }
    print(paste(co, matura, rok))
    colnames(dane_pow)[colnames(dane_pow)==matura] <- "wyniki"
    map <- ggplot(dane_pow, aes(long, lat, group=group, fill=wyniki)) + 
      geom_polygon(colour="black") + 
      labs(title=tytul) + 
      coord_equal() + 
      scale_fill_gradient2(name=paste(co,"(%)"), low=muted("red"), high=muted("blue"), midpoint=midpoint) +
      theme_opts
    ggsave(filename=paste0("../owoce/mapy/pow", "_", co, "_", matura, "_", rok, ".png"), plot=map)
  }
  
  #rysuj_pow("biologia_rozszerzona", "zdający")
  mapply(rysuj_pow, matura=c(matury,matury),
         co=c(rep("wyniki", length(matury)),rep("zdający", length(matury))))
}
