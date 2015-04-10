# do nowego pliku:
# Proby zabawy z wynikami - matura z biologii rozszerzona 
src = polacz()
bio_r_2014 <-pobierz_wyniki_egzaminu(src, "matura", "biologia rozszerzona", 2014, T)

bio_r_2014 <- bio_r_2014 %>% collect() # pobranie wyników z bazy danych na komputer
pytania <- bio_r_2014[,grep("^k_[0-9]*", names(bio_r_2014))]
sum_wynik <- rowSums(pytania)
procent_wynik <- 100 * sum_wynik/max(sum_wynik)
hist(procent_wynik)

ggplot(data.frame(sum_wynik), aes(x=sum_wynik)) + geom_histogram(colour="white", binwidth=1)


# Proby zabawy z wynikami - matura z polskiego podstawowa 
src = polacz()
pl_2014 <-pobierz_wyniki_egzaminu(src, "matura", "j. polski podstawowa", 2014, T)
pl_2014 <- pl_2014 %>% collect() # pobranie wyników z bazy danych na komputer
pytania <- pl_2014[,grep("^k_[0-9]*", names(pl_2014))]
sum_wynik <- rowSums(pytania, na.rm=T)
procent_wynik <- 100 * sum_wynik/max(sum_wynik)
ggplot(data.frame(sum_wynik), aes(x=sum_wynik)) +
  geom_histogram(colour="white", binwidth=1) +
  xlab("suma punktów") +
  ylab("liczba uczniów") +
  ggtitle("Wyniki matury podstawowej z j. polskiego w 2014 roku")

# inne
src = polacz()
uczniowie = pobierz_uczniow(src)
