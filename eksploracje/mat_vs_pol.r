library(mirt)
library(dplyr)
library(ggplot2)

mat_podst <- read.csv("../dane/wyniki/matematyka_podstawowa_2014.csv") 
pol_podst <- read.csv("../dane/wyniki/j._polski_podstawowa_2014.csv") 

mat_podst$suma <- mat_podst %>% select(starts_with("k_")) %>% rowSums(na.rm=T)
mat_podst <- mat_podst %>% select(-starts_with("k_"))

pol_podst$suma <- pol_podst %>% select(starts_with("k_")) %>% rowSums(na.rm=T)
pol_podst <- pol_podst %>% select(-starts_with("k_"))

ggplot(mat_podst, aes(x=suma)) + geom_histogram(binwidth = 1)
ggplot(pol_podst, aes(x=suma)) + geom_histogram(binwidth = 1)

# lub bez joinow by uniknac tez zmiany nazw?
pm <- inner_join(mat_podst, pol_podst, by='id_obserwacji') %>%
  rename(wynik_mat=suma.x, wynik_pol=suma.y, laureat_mat=laureat.x, laureat_pol=laureat.y)

qplot(wynik_mat, wynik_pol, data=pm, geom='bin2d',
      binwidth=c(1, 1), drop=FALSE)

ggplot(pm, aes(x=wynik_mat, y=..density.., fill=laureat_pol)) +
  geom_histogram(binwidth=1, alpha=0.5, position="identity")

ggplot(pm, aes(x=wynik_pol, y=..density.., fill=laureat_mat)) +
  geom_histogram(binwidth=1, alpha=0.5, position="identity")