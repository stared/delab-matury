library(mirt)

mat_podst <- read.csv("../dane/wyniki/matematyka_podstawowa_2014.csv")

mat_podst <- select(mat_podst, starts_with("k_")) 

fit <- mirt(mat_podst, 1)

# skalowanie skilla na punkty
plot(fit)

# krzywe dla zadan!
plot(fit, type = 'trace')
plot(fit, type = 'infotrace')
