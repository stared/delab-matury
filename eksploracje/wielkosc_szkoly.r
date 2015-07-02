rok <- 2014

szkoly <- read.csv(paste0("../dane/przetworzone/szkoly", rok, ".csv"))
matury <- read.csv(paste0("../dane/przetworzone/wyniki_przetworzone_", rok, ".csv"))
dane <- merge(matury, szkoly, by = "id_szkoly")

wielkosc_szkoly <- tapply()

print(dane$id_szkoly)