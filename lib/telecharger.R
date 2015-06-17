# Cette fonction telecharge un url, le sauvegarde dans un fichier.
# This function downloads a file from a url and saves it as "fichier".
# Ex: telecharger(url = "https://www.google.co.mz/images/nav_logo195.png", fichier = "./data/logo_google.png")

telecharger <- function(url, fichier) {
  if(!file.exists(fichier)){
    download.file(url = url, destfile = fichier, method = "curl")
    
  }
}
