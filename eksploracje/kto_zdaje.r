library(dplyr)
library(ggplot2)


# DANE DO POPRAWKI
#
wyniki <- read.csv("../dane/przetworzone/sumy_laureaty.csv")
#

kolumny <- grep("podstawowa|rozszerzona", colnames(wyniki), value = T)
liczba_zdajacych <- colSums(!is.na(wyniki[, kolumny]))

df <- data.frame(kolumny, liczba_zdajacych)

df$poziom <- "podstawowa"
df[grep("rozszerzona", df$kolumny), "poziom"] <- "rozszerzona"

df$przedmiot <- gsub("_podstawowa|_rozszerzona", "", df$kolumny) %>% gsub("_", " ", .)

df$przedmiot <- reorder(df$przedmiot, df$liczba_zdajacych)

df$przedmiot
ggplot(df, aes(x=factor(przedmiot), y=liczba_zdajacych/1000, fill=factor(poziom))) +
  coord_flip() +
  #scale_y_log10() + 
  geom_bar(stat='identity', position='dodge')

ggplot(wyniki[wyniki$rocznik > 1980 & wyniki$rocznik < 2000,], aes(rocznik)) + geom_histogram()

df$kobiety <- colSums(!is.na(wyniki[wyniki$plec == "k", kolumny]))
df$mezczyzni <- colSums(!is.na(wyniki[wyniki$plec == "m", kolumny]))
df$dyslektycy <- colSums(!is.na(wyniki[wyniki$dysleksja, kolumny]))
df$mlodzi <- colSums(!is.na(wyniki[wyniki$rocznik > 1995, kolumny]))
df$starzy <- colSums(!is.na(wyniki[wyniki$rocznik < 1995, kolumny]))

ggplot(df, aes(x=factor(przedmiot), y=kobiety/liczba_zdajacych, fill=factor(poziom))) +
  coord_flip() +
  geom_bar(stat='identity', position='dodge') +
  geom_hline(yintercept=0.5, color='black', linetype="longdash")

length(which(wyniki$plec == "k")) / nrow(wyniki[!is.na(wyniki$plec),])
#sporo brakujacych plci!
sum(is.na(wyniki$plec))/nrow(wyniki)

ggplot(df, aes(x=mezczyzni/1000, y=kobiety/1000, color=przedmiot, shape=poziom)) +
  # scale_color_brewer(palette="Set3") +
  geom_abline(linetype="longdash") +
  geom_point(alpha=0.7) +
  coord_fixed(ratio = 1) +
  scale_x_log10() +
  scale_y_log10()

ggplot(df, aes(x=factor(przedmiot), y=mlodzi/liczba_zdajacych, fill=factor(poziom))) +
  coord_flip() +
  geom_bar(stat='identity', position='dodge')


ggplot(df, aes(x=factor(przedmiot), y=starzy/liczba_zdajacych, fill=factor(poziom))) +
  coord_flip() + 
  geom_bar(stat='identity', position='dodge')


ggplot(df, aes(x=factor(przedmiot), y=dyslektycy/liczba_zdajacych, fill=factor(poziom))) +
  coord_flip() +
  geom_bar(stat='identity', position='dodge')

