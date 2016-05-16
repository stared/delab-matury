library(mirt)
library(dplyr)
library(ggplot2)

mat_podst <- read.csv("../dane/wyniki/matematyka_podstawowa_2014.csv")

mat_podst <- mat_podst %>%
  select(starts_with("k_")) %>%
  na.omit()

hist(mat_podst %>% rowSums, 51)

fit <- mirt(mat_podst, 1)
# fit <- mirt(mat_podst, 1, empiricalhist = TRUE)  # trwa kilka h

# skalowanie skilla na punkty
plot(fit)

# krzywe dla zadan!
plot(fit, type = 'trace')
plot(fit, type = 'infotrace')

summary(fit)
coef(fit)

simulation <- simdata(c(), c(), 20000, model = fit)  # to zakladaloby rozklad normalny, nie - rzeczywisty
hist(simulation %>% rowSums, 51)

thetas <- fscores(fit, full.scores = TRUE)
simulation_th <- simdata(c(), c(), -1, Theta = thetas, model = fit)
hist(simulation_th %>% rowSums, 51)

wyn1 <- mat_podst %>% rowSums %>% data.frame(suma = ., rodzaj = "rzeczywiste")
wyn2 <- simulation_th %>% rowSums %>% data.frame(suma = ., rodzaj = "symulacja")
wyns <- rbind(wyn1, wyn2)

ggplot(wyns, aes(x = suma, y = ..density.. * 100)) +
  geom_histogram(colour="white", binwidth=1, position="identity") +
  facet_grid(rodzaj ~ .) +
  xlab("liczba punktów") +
  ylab("% zdających") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ggtitle('model IRT')


# alternatywny wykres
ggplot(wyns, aes(x = suma, y = ..density.. * 100, fill = rodzaj)) +
  geom_histogram(binwidth=1, alpha=.5, position="identity") +
  xlab("liczba punktów") +
  ylab("% zdających") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  ggtitle('model IRT')

