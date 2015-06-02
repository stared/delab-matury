# http://zpd.ibe.edu.pl/doku.php?id=r_gr_szkoly

library(ZPD)
src <- polacz()
szkoly <- pobierz_szkoly(src)

# typ_szkoly == 'LO'
lata <- 2010:2014
sapply(lata, function(rok){
  szkoly_rok <- szkoly %>% filter(rok == rok) %>% collect()
  write.csv(szkoly_rok, paste0("../dane/szkoly", rok, ".csv", sep=""), row.names=FALSE)
})