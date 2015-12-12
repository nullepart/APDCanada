# Ce fichier sert à charger les données et les mettre dans un
# fichier binaire

# Une note sur la convention de codage: 
# fonctions:lowerCamelCase ; variables: noms séparés par des points


liste <- fread("./data/liste.fichiers.csv")  # fread importe un csv en data.table
liste <- liste[format == "csv" & language == "eng", ]


# On lit les fichiers référencés dans la 'liste' et, pour
# chacun, on les ajoute au tableau apd, qui deviendra le
# tableau-maitre.

mypath <- "./data/raw"
multmerge <- function(chemin) {
        # Fonction pour fusionner plusieurs tableaux csv
        filenames <- list.files(path = mypath, full.names = TRUE)
        datalist <- lapply(filenames, function(x) {
                read.csv(file = x, header = T, fileEncoding = "Latin1")
        })
        Reduce(function(x, y) {
                rbind(x, y)
        }, datalist)
}

apd <- multmerge(mypath)
apd <- data.table(apd)

# On sauvegarde le nom original des variables dans noms.apd
# puis on crée de nouveaux noms, qu'on repasse ensuite dans
# apd
noms.apd.orig <- names(apd)
noms.apd.nouv <- gsub(x = noms.apd.orig, pattern = "..", replacement = ".", 
                      fixed = T)
noms.apd.nouv <- gsub(x = noms.apd.nouv, pattern = "Organisation", 
                      replacement = "Org", fixed = F)
noms.apd.nouv <- gsub(x = noms.apd.nouv, pattern = "marker.", 
                      replacement = "", fixed = T)
noms.apd.nouv <- gsub(x = noms.apd.nouv, pattern = "[.]$", replacement = "")
setnames(apd, noms.apd.orig, noms.apd.nouv)
setnames(apd, "Fiscal.year", "FY")
setnames(apd, "Maximum.CIDA.contribution.project.level", "Budget")
setnames(apd, "Untied.amount.Project.level.budget", "Untied.amount")
setnames(apd, "Project.number", "No")

setkey(x = apd, "No")

# Nettoyage
apd <- apd[-which(apd$No == ""), ]  # Enlever les entrées vides
# Nettoyer les formats de diverses colonnes, incluant
# l'exercice fiscal (FY)
apd <- apd %>% mutate(FY = as.numeric(str_sub(FY, start = -4))) %>% 
        mutate(Budget = as.numeric(extract_numeric(Budget))) %>% 
        mutate(Untied.amount = as.numeric(extract_numeric(Untied.amount))) %>% 
        mutate(Country.region.percent = as.numeric(extract_numeric(Country.region.percent))/100) %>% 
        mutate(Sector.percent = as.numeric(extract_numeric(Sector.percent))/100) %>% 
        mutate(Amount.spent = as.numeric(extract_numeric(Amount.spent)))

# Nettoyage
rm(list = c("mypath", "noms.apd.nouv", "noms.apd.orig"))

# On met extrait les secteurs du tableau apd pour les mettre
# dans sect
sect.var <- c("Sector.ID", "Sector.name")
sect <- apd %>% select(one_of(sect.var)) %>%
        unique(by = "Sector.ID") %>%
        arrange(Sector.ID)
setkey(sect, "Sector.ID")

# Même chose avec les pays

pays.var <- c("Continent.ID", "Continent.name", "Country.region.ID", 
              "Project.Browser.country.ID", "Country.region.name")

pays <- apd %>% select(one_of(pays.var)) %>% 
        unique(by = c("Project.Browser.country.ID")) %>%
        arrange(Country.region.name)
setkey(pays, "Project.Browser.country.ID")

# Même chose avec les organisations
org.var <- c("Org.ID", "Org.name", 
             "Org.type.location.profit.not.for.profit", "Org.class", "Org.sub.class")
org <- apd %>% select(one_of(c("FY", "No", org.var))) %>% 
        group_by(No, FY, Org.ID) %>% 
        unique(by = "Org.ID") %>%
        arrange(Org.ID)
org <- org %>% separate(Org.type.location.profit.not.for.profit, c("Origine", "Type"), sep = " ", extra = "drop", fill = "warn")

org[Origine == "Uncoded", Origine := NA]
org[Org.class == "Unknown", Org.class := NA]
org[Org.sub.class == "Unknown", Org.sub.class := NA]
setkey(org, "No")

# Même chose avec les projets

proj.var <- c("Status", "Amount.spent", "Budget", "Branch.ID", "Branch.name", "Division.ID", "Division.name", "Section.ID", "Section.name", "Regional.program", "Fund.centre.ID", "Fund.centre.name", "Untied.amount", "FSTC.percent", "IRTC.percent", "CFLI", "Type.of.aid.ID", "Type.of.aid", "CIDA.business.delivery.model.old", "Programming.process.new", "Bilateral.aid.international", "PBA.type", "Environmental.sustainability", "Climate.change.adaptation", "Climate.change.mitigation", "Desertification", "Biodiversity", "Participatory.development.and.good.governance", "Trade.development", "Urban.issues", "Children.issues", "Youth.issues", "Indigenous.issues", "Disability.issues", "ICT.as.a.tool.for.development", "Knowledge.for.development", "Gender.equality", "Sector.percent", "Country.region.percent")
proj <- apd %>% select(one_of(c("FY", "No", proj.var))) %>% 
        group_by(No, FY) %>% 
        mutate(Amount.spent = sum(Amount.spent)) %>% 
        unique(by = c("FY", "No")) %>%
        arrange(No, FY)

# On rajoute une année de début et une année de fin à chaque projet
duree <- proj %>% group_by(No) %>% summarise(debut = min(FY), fin = max(FY))

# Comme il est impossible de savoir si un projet a bien commencé en 2006 ou s'est
# bien terminé en 2013, on exclut ces années du calcul de la durée.
duree[debut == 2006, "debut"] <- NA
duree[fin == 2013, "fin"] <- NA
duree <- duree %>% mutate(annees = (fin - debut + 1))
temp <- !is.na(duree$annees)
duree <- duree[temp]


# Sauvegarder dans un fichier binaire
save(apd, duree, liste, org, pays, proj, sect, compress = T, file = "./data/donnees.RData")

# Nettoyage
rm(list = c("org.var", "pays.var", "proj.var", "sect.var", "temp", "liste", "multmerge"))
