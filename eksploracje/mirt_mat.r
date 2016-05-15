library(mirt)
library(dplyr)

mat_podst <- read.csv("../dane/wyniki/matematyka_podstawowa_2014.csv")

mat_podst <- select(mat_podst, starts_with("k_"))
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



