# Fonction pour voir un échantillon de tableau

voir <- function(td, n = 15) {
  a <- dim(td)[2]
  View(td[sample(1:a, n)])
}