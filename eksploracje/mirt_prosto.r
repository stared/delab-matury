library(dplyr)
library(mirt)
library(ggplot2)

df <- read.csv("../dane/przetworzone/sumy_laureaty.csv")

# jeszcze trzeba usunac laureatow

przedmioty <- df %>%
  select(matches("podst|rozsz"))

maksima <- przedmioty %>% summarise_each(funs(max(., na.rm = TRUE)))
mediany <- przedmioty %>% summarise_each(funs(median(., na.rm = TRUE)))

str(maksima)

przeskalowane <- 100 * przedmioty/t(maksima)


zdal <- lapply(przeskalowane, function (x) {as.numeric(x > 30)}) %>% as.data.frame

przeskalowane_inaczej <-  przedmioty/t(mediany)
lepiej <- lapply(przeskalowane_inaczej, function (x) {as.numeric(x > 1)}) %>% as.data.frame

fit <- mirt(zdal, 1)
plot(fit, type = 'trace')
plot(fit, type = 'infotrace')

summary(fit)

fit_lepiej <- mirt(lepiej, 1)
plot(fit_lepiej, type = 'trace')
plot(fit_lepiej, type = 'infotrace')



cs <- coef(fit)
cs <- coef(fit_lepiej)

wspolczynniki <- data.frame()
for (x in names(cs)) {
  wspolczynniki[x, 'przedmiot'] <- x
  wspolczynniki[x,'a'] <- cs[[x]][[1]]
  wspolczynniki[x,'d'] <- cs[[x]][[2]]
} 

wspolczynniki <- wspolczynniki %>%
  filter(przedmiot != "GroupPars")  %>%
  mutate(sigma=1/a, thre=-d/a) %>%
  mutate(przedmiot=reorder(przedmiot, thre))

limits <- aes(ymax = thre + sigma, ymin=thre - sigma)
ggplot(wspolczynniki, aes(x=factor(przedmiot), y=thre)) +
  geom_point() +
  geom_errorbar(limits) +
  coord_flip()
