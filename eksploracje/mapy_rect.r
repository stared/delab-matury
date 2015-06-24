library(maptools)
library(rgdal)
library(ggplot2)
library(dplyr)
library(scales)
library(ggvis)

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

# wczytanie danych
rok <- 2014
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

write.csv(dane_woj, "wojewodztwa.csv")

# wykresy

# własności graficzne wykresu
theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="white"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

rysuj_woj_kola <- function(matura){
  tytul <- paste0("Wyniki w ", rok, ": ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
  dane_woj <- merge(centroidy_df, srWoj[,c("id", matura)], by="id")
  colnames(dane_woj)[colnames(dane_woj)==matura] <- "wyniki"
  dane_woj <- merge(dane_woj, liczWoj[,c("id", matura)], by="id")
  colnames(dane_woj)[colnames(dane_woj)==matura] <- "zdający"
  midpoint <- srPL[matura]
  print(paste(matura, rok))
  map <- ggplot(dane_woj, aes(map_id = id)) + 
    geom_map(colour= "#888888", map = woj_df, fill="#eeeeee") +
    geom_point(data=dane_woj, 
               aes(x=Longitude, y=Latitude, size=zdający, fill=wyniki), pch=21) +
    scale_size_area(max_size=12) +
    labs(size="liczba zdających", fill="śr. wynik (%)") +
    scale_fill_gradient2(low="red", high="blue", midpoint=midpoint) +
    expand_limits(x = woj_df$long, y =woj_df$lat) +
    labs(title=tytul) + 
    coord_equal() + 
    theme_opts 
  #ggsave(filename=paste0("../owoce/mapy/woj", "_", co, "_", matura, "_", rok, ".png"), plot=map)
  return(map)
}

rysuj_woj_kola("biologia_rozszerzona")

# wersja w ggvis

woj_df %>%
  ggvis(~long, ~lat) %>%
  group_by(group, id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>%
  hide_legend("fill") %>%
  hide_axis("x") %>% hide_axis("y") %>%
  set_options(width=400, height=600, keep_aspect=TRUE)
