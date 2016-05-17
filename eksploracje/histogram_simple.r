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

# pewnie da sie prosciej w R
sumyPolLata <- rbind(
  histogramSumyDf("j._polski_podstawowa", 2010),
  histogramSumyDf("j._polski_podstawowa", 2011),
  histogramSumyDf("j._polski_podstawowa", 2012),
  histogramSumyDf("j._polski_podstawowa", 2013),
  histogramSumyDf("j._polski_podstawowa", 2014))

sumyPolLata %>%
  group_by(rok) %>%
  summarise(
    wynik_min = min(suma),
    wynik_max = max(suma),
    liczba_zdajacych = sum(liczba)
  )

ggplot(sumyPolLata %>% filter(suma < 30), aes(x = suma, y = liczba)) +
  geom_bar(stat = "identity", color = "white", position = "identity") +
  facet_grid(rok ~ .) +
  xlab("total score [%]") +
  ylab("number of students") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ggtitle("matura exam: Polish, basic level")


21 / 74
