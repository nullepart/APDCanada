# Les données sur l'aide publique au développement (APD, ODA en anglais)
# du gouvernement canadien se trouvent ici:
# http://ouvert.canada.ca/data/fr/dataset/40f57c8a-58a9-47c8-b9ff-2e3090044137

# Les métadonnées sont contenues dans un fichier JSON

# Sys.setlocale("LC_ALL", "fr_CA.UTF-8")

url <- "http://ouvert.canada.ca/data/api/action/package_show?id=40f57c8a-58a9-47c8-b9ff-2e3090044137"
json.data <- fromJSON(url, simplifyDataFrame = TRUE)

df.json <- json.data$result$resources
names(df.json) <- make.names(names(df.json))

dt <- df.json %>%
  select(id, format, name, name_fra, language, created, url) %>%
  mutate(language = str_sub(language, 1, 3)) %>%
  mutate(nom.fichier = make.names(paste("apd", name, language, id, tolower(format), sep = "."))) %>%
  mutate(format = tolower(format))

write.csv(dt, "./data/liste.fichiers.csv")

# On attribue le chemin ./doc/ pour la documentation et ./data/raw/ pour les csv
# en se servant des propriétés de fusion de tableau de data.table

dt <- data.table(dt)
setkey(dt, format)

dt.temp <- data.table(format = c("csv", "html"), chemin = c("./data/raw/", "./doc/"))
setkey(dt.temp, format)

dt <- dt.temp[dt]

# On utilise d_ply pour exécuter la fn télécharger, définie dans ./lib

d_ply(dt[dt$language == "eng"], "url", function(x) {
  url <- x$url
  fichier <- paste(x$chemin, x$nom.fichier, sep="")
  telecharger(url, fichier)
}
)

# Effacer ce qui suit? Est-ce encore utile?

# dt <- data.table(
#   df2 <- df %>%
#     select(language, url, nom.fichier) %>%
#     filter(language == "eng")
#   )
# 

