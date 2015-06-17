# Ce fichier sert à charger les données et les mettre dans un fichier binaire

liste <- fread("./data//liste.fichiers.csv") #fread importe un csv en data.table

apd <- data.table()

charger.td <- function(x) {
  fichier <- paste("./data/original/", x$nom.fichier, sep="")
  temp.df <- read.csv(fichier)
  rbind(apd, fichier)
}

d_ply(liste[liste$language == "eng" & liste$format == "csv", ], "nom.fichier", function(x) {
  fichier <- paste("./data/original/", x$nom.fichier, sep="")
  nom <- paste0("apd", x$name)
  assign(nom, read.csv(fichier, nrow =10))
}
)

