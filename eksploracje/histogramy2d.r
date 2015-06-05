library(dplyr)
library(ggplot2)

wyniki <- read.csv("../dane/przetworzone/sumy_laureaty.csv")

ggplot(wyniki, aes(j_polski_podstawowa, matematyka_podstawowa)) +
  geom_bin2d(binwidth=c(1, 1), drop=FALSE)
