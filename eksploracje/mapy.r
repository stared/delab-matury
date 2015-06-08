library(maptools)
library(rgdal)
library(ggplot2)
library(dplyr)
library(scales)

# TODO: w legendzie: bd.


## kontury powiatów
powiaty <- readOGR(dsn="../dane/PRG_jednostki_administracyjne_v8/powiaty.shp", layer="powiaty")

powiaty_df <- fortify(powiaty, region = "jpt_nazwa_")
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
wojewodztwa <- readShapeSpatial("../dane/PRG_jednostki_administracyjne_v8/wojew¢dztwa.shp")

woj_df <- fortify(wojewodztwa, region = "jpt_nazwa_")
woj_df$id = iconv(woj_df$id, "windows-1250", "UTF-8") # polskie znaki
woj_df$group = iconv(woj_df$group, "windows-1250", "UTF-8")


# wyniki matur z podziałem na wojewodztwa i na powiaty
szkoly <- read.csv("../dane/szkoly2014.csv")
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

# średnie wyniki dla województw
sapply(matury, function(nazwa){
  print(nazwa)
  max <- max(dane[,colnames(dane)==nazwa], na.rm=T)
  #print(max)
  tapply(dane[,colnames(dane)==nazwa]/max*100, dane$wojewodztwo_szkoly, mean, na.rm=T)
  }) %>% data.frame(.) -> srWoj

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

#setdiff(unique(powiaty_df$id),rownames(srPow))
# problem: brak maturzystów w powiecie siedleckim?


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
rysuj_woj <- function(matura){
  tytul <- paste0("Średnie wyniki: ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
  srednie <- data.frame(wyniki=srWoj[,colnames(srWoj)==matura], id=rownames(srWoj))
  colnames(srednie) <- c("wyniki", "id")
  srednie_woj<-merge(woj_df, srednie, by="id", sort = FALSE, all.x=TRUE)
  srednia <- srPL[matura]
  print(srednia)
  map <- ggplot(srednie_woj, aes(long,lat, group=group, fill=wyniki)) + 
    geom_polygon(colour="black") + 
    labs(title=tytul) + 
    coord_equal() + 
    scale_fill_gradient2(name="wyniki (%)", low=muted("red"), high=muted("blue"), midpoint=srednia) +
    theme_opts
  return(map)
}

rysuj_woj("j_polski_podstawowa")

# rysowanie mapy ze średnimi wynikami w powiatach
rysuj_pow <- function(matura){
  tytul <- paste0("Średnie wyniki: ", gsub("^j_", "j. ", matura) %>% gsub("_", " ", .))
  srednie <- data.frame(wyniki=srPow[,colnames(srPow)==matura], id=rownames(srPow))
  colnames(srednie) <- c("wyniki", "id")
  srednie_pow<-merge(powiaty_df, srednie, by="id", sort = FALSE, all.x=TRUE)
  order <- order(srednie_pow$order)
  srednie_pow <- srednie_pow[order,]
  srednia <- srPL[matura]
  print(srednia)
  map <- ggplot(srednie_pow, aes(long, lat, group=group, fill=wyniki)) + 
    geom_polygon(colour="black") + 
    labs(title=tytul) + 
    coord_equal() + 
    scale_fill_gradient2(name="wyniki (%)", low=muted("red"), high=muted("blue"), midpoint=srednia) +
    theme_opts
  return(map)
}

rysuj_pow("j_polski_podstawowa")
