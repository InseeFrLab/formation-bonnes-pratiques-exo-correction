
decennie_a_partir_annee <- function(annee) return(annee - annee %% 10)


#' Fonction de Statistique Agrégée
#'
#' Calcule une statistique agrégée (moyenne, écart-type ou variance) pour un vecteur numérique donné.
#'
#' @param x Un vecteur numérique de valeurs pour lequel la statistique agrégée sera calculée.
#' @param stat Une chaîne de caractères spécifiant le type de statistique à calculer. Options possibles :
#'   \itemize{
#'     \item `"moyenne"` : Calcule la moyenne (par défaut).
#'     \item `"ecart-type"` ou `"sd"` : Calcule l'écart-type.
#'     \item `"variance"` : Calcule la variance.
#'   }
#' @param ... Arguments supplémentaires passés aux fonctions sous-jacentes (par exemple, `mean`, `sd`, `var`).
#'
#' @return Une valeur numérique représentant la statistique calculée.
#'
#' @examples
#' # Calculer la moyenne d'un vecteur normal aléatoire
#' fonction_de_stat_agregee(rnorm(10))
#'
#' # Calculer l'écart-type d'un vecteur normal aléatoire
#' fonction_de_stat_agregee(rnorm(10), "ecart-type")
#'
#' # Calculer la variance d'un vecteur normal aléatoire
#' fonction_de_stat_agregee(rnorm(10), "variance")
#'
#' @export
stats_desc_variable <- function(x, stat = "moyenne", ...) {
  
  if (stat == "moyenne") {
    x <- mean(x, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    x <- sd(x, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    x <- var(x, na.rm = TRUE, ...)
  }
  return(x)
  
}
