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

# lub bez joinow
pm = inner_join(mat_podst, pol_podst, by='id_obserwacji')

qplot(suma.x, suma.y, data=pm, geom='bin2d',
      bins=c(max(pm$suma.x) + 1, max(pm$suma.y) + 1), drop=FALSE)


