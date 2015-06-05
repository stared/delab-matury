library(maptools)
library(rgdal)
library(ggplot2)

## kontury powiatów
powiaty <- readOGR(dsn="../dane/PRG_jednostki_administracyjne_v8/powiaty.shp", layer="powiaty")

powiaty_df <- fortify(powiaty, region = "jpt_nazwa_")
powiaty_df$id = iconv(powiaty_df$id, "windows-1250", "UTF-8")
powiaty_df$id = gsub("powiat ", "", powiaty_df$id)
powiaty_df$group = iconv(powiaty_df$group, "windows-1250", "UTF-8")
powiaty_df$group = gsub("powiat ", "", powiaty_df$group)

#test rysowania map
losowe <- data.frame(losowe=rnorm(unique(powiaty_df$id)), id=unique(powiaty_df$id)) 
powiaty2<-merge(powiaty_df, losowe, by="id")
map<- qplot(long, lat, data=powiaty2, group=group , fill=losowe, geom="polygon")

# problem: powiaty o tej samej nazwie w różnych województwach!

# województwa
wojewodztwa <- readOGR(dsn="../dane/PRG_jednostki_administracyjne_v8/wojew¢dztwa.shp",
                       layer="wojew¢dztwa")
wojewodztwa <- readShapeSpatial("../dane/PRG_jednostki_administracyjne_v8/wojew¢dztwa.shp")

woj_df <- fortify(wojewodztwa, region = "jpt_nazwa_")
woj_df$id = iconv(woj_df$id, "windows-1250", "UTF-8") # polskie znaki
#woj_df$id = gsub("powiat ", "", powiaty_df$id)
woj_df$group = iconv(woj_df$group, "windows-1250", "UTF-8")

#test rysowania map
losowe <- data.frame(losowe=rnorm(unique(woj_df$id)), id=unique(woj_df$id)) 
woj2<-merge(losowe, woj_df, by="id", sort = FALSE)
map<- qplot(long, lat, data=woj2, group=group , fill=losowe, geom="polygon")
map0 <- qplot(long, lat, data=woj_df, group=group , fill=id, geom="polygon")


# wyniki matur z podziałem na powiaty
szkoly <- read.csv("../dane/szkoly2014.csv")
wyniki <- read.csv("../dane/przetworzone/sumy_laureaty.csv")

dane <- merge(wyniki, szkoly, by = "id_szkoly")

