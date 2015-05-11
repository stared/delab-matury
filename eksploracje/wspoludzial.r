library(ggplot2)
library(dplyr)
library(igraph)

# DANE DO POPRAWKI
#
wyniki <- read.csv("../dane/przetworzone/sumy_laureaty.csv")

kolumny = grep("podstawowa|rozszerzona", colnames(wyniki))

koincydencje <- wyniki %>% select(matches("podstawowa|rozszerzona")) %>%
  select(-matches("polski_p|matematyka_p|angielski_p"))  %>%
  is.na %>% `!` %>%
  as.matrix %>% crossprod

g <- graph.adjacency(koincydencje, weighted=TRUE, mode="undirected", diag = FALSE)

V(g)$size <- sqrt(diag(koincydencje)/1000)

E(g)$width <- 2*sqrt(E(g)$weight/1000)

# layout.kamada.kawai
# layout.fruchterman.reingold

# nie wiem czemu nie moge zrobic z tego ladnego, wagowanego przyciagania...
pozycje <- layout.spring(g, params=list(weights=sqrt(E(g)$weight/100), niter=10000))

plot(g,
     layout=pozycje,
     vertex.label.dist=0.5)
