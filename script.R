# ENVIRONNEMENT -------------------------------

library(dplyr)
library(ggplot2)


api_token <- Sys.getenv("JETON_API")
if (api_token == ""){
  api_token <- rstudioapi::askForPassword("Renseigner le jeton d'API")
}


# FONCTIONS ==================================

decennie_a_partir_annee <- function(annee) return(annee - annee %% 10)

#' Fonction de Statistique Agrégée
#'
#' Calcule une statistique agrégée (moyenne, écart-type ou variance) pour un vecteur numérique donné.
#'
#' @param a Un vecteur numérique de valeurs pour lequel la statistique agrégée sera calculée.
#' @param b Une chaîne de caractères spécifiant le type de statistique à calculer. Options possibles :
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
fonction_de_stat_agregee <- function(a, b = "moyenne", ...) {
  
  if (b == "moyenne") {
    x <- mean(a, na.rm = TRUE, ...)
  } else if (b == "ecart-type" || b == "sd") {
    x <- sd(a, na.rm = TRUE, ...)
  } else if (b == "variance") {
    x <- var(a, na.rm = TRUE, ...)
  }
  return(x)
  
}


# IMPORT --------------------------------------------

df <- readr::read_csv2(
  "individu_reg.csv",
  col_select = c(
    "region", "aemm", "aged", "anai", "catl",
    "cs1", "cs2", "cs3", "couple", "na38", "naf08",
    "pnai12", "sexe", "surf", "tp", "trans", "ur"
  )
)


# NETTOYAGES --------------------------------------------

df <- df %>%
  mutate(aged = as.numeric(aged)) %>%
  mutate(sexe = as.character(sexe)) %>%
  mutate(sexe = forcats::fct_recode(sexe, Homme = "1", Femme = "2"))


stats_transport_age <- df %>%
  group_by(couple, trans) %>%
  summarise(x = n()) %>%
  group_by(couple) %>%
  mutate(y = 100 * x / sum(x))


part_hommes_cohortes <- df %>%
  group_by(aged, sexe) %>%
  summarise(SH_sexe = n()) %>%
  group_by(aged) %>%
  mutate(SH_sexe = SH_sexe / sum(SH_sexe)) %>%
  filter(sexe == 1)


# STATISTIQUES DESCRIPTIVES ------------------------------

# STATISTIQUES UNIVARIEES ============================

fonction_de_stat_agregee(df %>% filter(sexe == "Homme") %>% pull(aged))
fonction_de_stat_agregee(df %>% filter(sexe == "Femme") %>% pull(aged))


# DISTRIBUTION D'AGE =================================

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")


p <- part_hommes_cohortes %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

p
ggsave("p.png", p)




# MODELISATION ----------------------------------------

echantillon_modelisation <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z") %>%
  mutate(surf = factor(surf, ordered = TRUE), cs1 = factor(cs1))

MASS::polr(surf ~ cs1 + factor(ur), echantillon_modelisation)
