library(dplyr)
library(ggplot2)

histogramSumyDf <- function (przedmiot, rok) {
  sciezka <- paste0("../dane/wyniki/", przedmiot, "_", rok, ".csv")
  read.csv(sciezka) %>%
    select(starts_with("k_")) %>%
    rowSums(na.rm = T) %>%
    data.frame(suma = .) %>%
    group_by(suma) %>%
    summarise(liczba = n(), przedmiot = przedmiot, rok = rok)
}

sumyPolLata <- 2010:2014 %>%
  lapply(function(year) { histogramSumyDf("j._polski_podstawowa", year) }) %>%
  rbind_all

sumyPolLata %>%
  group_by(rok) %>%
  summarise(
    wynik_min = min(suma),
    wynik_max = max(suma),
    liczba_zdajacych = sum(liczba)
  )

officalMax <- 70  # officially, everything 70-74 counts as 70

ggplot(sumyPolLata, aes(x = 100 * suma / officalMax, y = liczba / 1000)) +
  geom_bar(stat = "identity", color = "white", position = "identity") +
  facet_grid(rok ~ .) +
  xlab("total score [%]") +
  ylab("number of students [thousands]") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ggtitle("matura exam in Poland: Polish language, basic level")
