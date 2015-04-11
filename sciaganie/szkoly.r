# http://zpd.ibe.edu.pl/doku.php?id=r_gr_szkoly

library(ZPD)
src <- polacz()
szkoly <- pobierz_szkoly(src)

# typ_szkoly == 'LO'
szkoly2014 <- szkoly %>% filter(rok == 2014) %>% collect()

write.csv(szkoly2014, "../dane/szkoly2014.csv", row.names=FALSE)