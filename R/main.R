# ENVIRONNEMENT -------------------------------

library(dplyr)
library(ggplot2)

source("R/functions.R", encoding = "UTF-8")

api_token <- Sys.getenv("JETON_API")
if (api_token == ""){
  api_token <- rstudioapi::askForPassword("Renseigner le jeton d'API")
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

stats_desc_variable(
  df %>% filter(sexe == "Homme") %>% pull(aged)
)
stats_desc_variable(
  df %>% filter(sexe == "Femme") %>% pull(aged)
)


# DISTRIBUTION D'AGE =================================

ggplot(df) +
  geom_histogram(aes(x = 5 * floor(as.numeric(aged) / 5)), stat = "count")


p <- part_hommes_cohortes %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

p
ggsave("output/part_hommes_cohortes.png", p)




# MODELISATION ----------------------------------------

echantillon_modelisation <- df %>%
  select(surf, cs1, ur, couple, aged) %>%
  filter(surf != "Z") %>%
  mutate(surf = factor(surf, ordered = TRUE), cs1 = factor(cs1))

MASS::polr(surf ~ cs1 + factor(ur), echantillon_modelisation)
