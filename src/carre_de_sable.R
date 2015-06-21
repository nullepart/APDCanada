# Carré de sable pour du code utile

test <- data.table(variable = names(apd))
test$type <- NA
for(i in 1:8){
      fichier <- paste0("./data/raw/", liste$nom.fichier[i])
      nom <- paste0("apd", i)
      assign(nom, read.csv(fichier, nrow=10))
}


for(i in 1:dim(apd)[2]) {
      test$type[i] <- class(apd[[i]])
}
