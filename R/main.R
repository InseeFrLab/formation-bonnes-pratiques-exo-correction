# ENVIRONNEMENT -------------------------------

library(arrow)
library(dplyr)
library(ggplot2)

source("R/functions.R", encoding = "UTF-8")

api_token <- Sys.getenv("JETON_API")
if (api_token == ""){
  api_token <- rstudioapi::askForPassword("Renseigner le jeton d'API")
}


# IMPORT --------------------------------------------

columns_subset <- c(
  "REGION", "AGED", "ANAI", "CATL", "COUPLE",
  "SEXE", "SURF", "TP", "TRANS"
)


df <- open_dataset(
  "./data/RPindividus",
  hive_style = TRUE
) %>%
  filter(REGION == 24) %>%
  select(any_of(columns_subset)) %>%
  collect()

df <- df %>%
  rename_with(tolower) %>%
  as_tibble()


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
  filter(sexe == "Homme")


# STATISTIQUES DESCRIPTIVES ------------------------------

# STATISTIQUES UNIVARIEES ============================

stats_desc_variable(
  df %>% filter(sexe == "Homme") %>% pull(aged)
)
stats_desc_variable(
  df %>% filter(sexe == "Femme") %>% pull(aged)
)


# DISTRIBUTION D'AGE =================================

p <- part_hommes_cohortes %>%
  ggplot() +
  geom_bar(aes(x = aged, y = SH_sexe), stat = "identity") +
  geom_point(aes(x = aged, y = SH_sexe), stat = "identity", color = "red") +
  coord_cartesian(c(0, 100))

p
ggsave("output/part_hommes_cohortes.png", p)



# MODELISATION ----------------------------------------

echantillon_modelisation <- df %>%
  filter(surf != "Z") %>%
  sample_n(1000)

MASS::polr(surf ~ couple + sexe, echantillon_modelisation)
