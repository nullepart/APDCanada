# Ce fichier sert à charger les données et les mettre dans un fichier binaire

liste <- fread("./data/liste.fichiers.csv") # fread importe un csv en data.table
liste <- liste[format == "csv" & language == "eng", ]


# On lit les fichiers référencés dans la "liste" et, pour chacun, on les ajoute
# au tableau apd, qui deviendra le tableau-maitre.

# On commence par initialiser apd

apd <- data.table()

# Lire chaque tableau référencé par la liste


apd <- ddply(liste, "nom.fichier", function(x) {
      fichier <- paste0("./data/raw/", x$nom.fichier)
      nom <- paste0("apd", x$name)
      assign(nom, fread(fichier, nrow =10))
}
)
apd <- data.table(apd)

# On sauvegarde le nom original des variables dans noms.apd
# puis on crée de nouveaux noms, qu'on repasse ensuite dans apd
noms.apd <- data.table(original = names(apd), nouv = make.names(names(apd)))
noms.apd$nouv <- gsub(x = noms.apd$nouv, pattern = "..", replacement = ".", fixed = T)
names(apd) <- noms.apd$nouv

setkey(x = apd, "Project.number")






