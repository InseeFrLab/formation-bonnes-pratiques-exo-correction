library(fs)
library(tictoc)
library(profvis)
library(DBI)
library(duckdb)
library(gt)

# PARAMETERS --------------------------------------------

columns_subset <- c(
  "region", "aged", "anai", "catl",
  "couple",
   "sexe", "surf", "tp", "trans"
)
columns_subset <- toupper(columns_subset)

filename_sample_csv <- "data/RPindividus_24.csv"
filename_sample_parquet <- gsub("csv", "parquet", filename_sample_csv)
filename_full_parquet <- gsub("_24", "", filename_sample_parquet)


# FUNCTIONS --------------------------------------------


file_size <- function(file_path) {
  size_in_bytes <- file.info(file_path)$size
  return(
    fs::fs_bytes(size_in_bytes)
  )
}

import_time_csv <- function(path, import_fun = .f, ...) {

  start_time <- Sys.time()
  csv_data <- import_fun(path, ...)
  end_time <- Sys.time()
  diff_time <- end_time - start_time
  
  return(diff_time)

}

import_time_parquet <- function(path, col_names = NULL) {
  
  start_time <- Sys.time()
  parquet_data <- open_dataset(path)
  
  if (!is.null(col_names)){
    parquet_data <- parquet_data %>%
      select(any_of(col_names))
  }
  
  parquet_data <- parquet_data %>% collect()
  
  end_time <- Sys.time()
  diff_time <- end_time - start_time
  
  return(diff_time)
  
}

# DISK FILESIZE ------------------------------------------------

disk_usage_sample_csv <- file_size(filename_sample_csv)
disk_usage_sample_parquet <- file_size(filename_sample_parquet)
disk_usage_full_parquet <- file_size(filename_full_parquet)


# IMPORT TIME --------------------------------------------------

# CSV (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ====================
diff_time_csv <- import_time_csv(filename_sample_csv, readr::read_csv)
diff_time_csv_subset <- import_time(filename_sample_csv, readr::read_csv, col_names = columns_subset)

# PARQUET (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ===============
diff_time_parquet <- import_time_parquet(filename_sample_parquet)  
diff_time_parquet_subset <- import_time_parquet(
  filename_sample_parquet,
  col_names = columns_subset 
)

# PARQUET (FULL): WITH AND WITHOUT COLUMN CONDITIONING ===============
diff_time_parquet_full <- import_time_parquet(filename_full_parquet)
diff_time_parquet_full_sample <- import_time_parquet(
  filename_full_parquet,
  col_names = columns_subset
)


# PROFILING: VISUALISING TIME SPENT ON DIFFERENT OPERATIONS ----------------------


# CSV (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ====================
profvis(readr::read_csv(filename_sample_csv), interval = .005)
profvis(
  readr::read_csv(filename_sample_csv, col_names = columns_subset), interval = .005
)
# ce que ça nous dit: globalement le temps en lecture change pas (logique: csv)


# PARQUET (SAMPLE): WITH AND WITHOUT COLUMN CONDITIONING ===============
profvis(
  open_dataset(filename_sample_parquet) %>% collect(),
  interval = .005
)
profvis(
  open_dataset(filename_sample_parquet) %>%
    select(any_of(columns_subset)) %>%
    collect(),
  interval = .005)
# ça va vite mais on voit déjà une diff: si on passait sur le gros fichier ? ;) 


# PARQUET (FULL): WITH AND WITHOUT COLUMN CONDITIONING ===============
profvis(
  open_dataset(filename_full_parquet) %>% collect(),
  interval = .005
)
profvis(
  open_dataset(filename_full_parquet) %>%
    select(any_of(columns_subset)) %>%
    collect(),
  interval = .005
)

# DIMENSIONS ---------------------------------------------------------

complete <- open_dataset(filename_full_parquet) %>% collect()
sample <- open_dataset(filename_sample_parquet) %>% collect()

cols_complete %>%

# ON MET TOUT ENSEMBLE ------------------------------------------------

# Création du dataframe avec une troisième colonne pour indiquer l'échantillon
results_df <- data.frame(
  format = rep(
    c(
      "CSV 🐢", 
      "Parquet 🐎", 
      "Parquet 🐎"
    ), 
    each = 2
  ),
  cols = rep(
    c("Toutes les colonnes", "Sous-ensemble de colonnes"),
    times = 3
  ),
  disk = rep(
    c(
      as.numeric(gsub("M", "", disk_usage_sample_csv)),
      as.numeric(gsub("M", "", disk_usage_sample_parquet)),
      as.numeric(gsub("M", "", disk_usage_full_parquet))
    ), 
    each = 2
  ),
  import = c(
    as.numeric(diff_time_csv),
    as.numeric(diff_time_csv_subset),
    as.numeric(diff_time_parquet),
    as.numeric(diff_time_parquet_subset),
    as.numeric(diff_time_parquet_full),
    as.numeric(diff_time_parquet_full_sample)
  ),
  sample = c(
    rep("✔️", 4),
    "❌️", "❌️")

)

results_df <- results_df %>%
  mutate(import_bar = import, disk_bar = disk) %>%
  select(order(colnames(.))) %>%
  select(format, cols, sample, everything())

gt(
  results_df
  ) %>%
  fmt_markdown(columns = 'sample') %>%
  fmt_number(columns = "import", decimals = 2) %>%
  fmt_number(columns = "disk", decimals = 0) %>%
  gtExtras::gt_plt_bar(
    column = import_bar
  ) %>%
  gtExtras::gt_plt_bar(
    column = disk_bar
  ) %>%
  tab_spanner(label = md("Configuration"), columns = c("cols", "sample") ) %>%
  tab_spanner(label = md("Taille sur disque _(MiB)_"), columns = starts_with("disk")) %>%
  tab_spanner(label = md("Vitesse à l'import _(sec)_"), columns = starts_with("import")) %>%
  cols_label(
    format = "Format du fichier",
    cols = "Colonnes",
    sample = "Echantillon de données ?",
    disk = "",
    import = "",
    ends_with("_bar") ~ "",
    .fn = md
  )


# COMPRENDRE AVEC EXPLAIN ANALYZE --------------

url_table_individu <- "https://static.data.gouv.fr/resources/recensement-de-la-population-fichiers-detail-individus-localises-au-canton-ou-ville-2020-1/20231023-122841/fd-indcvi-2020.parquet"

con <- dbConnect(duckdb())

dbExecute(
  con,
  glue(
    "INSTALL httpfs;",
    "LOAD httpfs;"
  )
)



plan_base_complete <- dbGetQuery(con,
           glue(  
             'EXPLAIN ANALYZE ',
             'SELECT * FROM read_parquet("{url_table_individu}")'
           )
)

plan_peu_de_lignes <- dbGetQuery(
  con,
  glue(  
    'EXPLAIN ANALYZE ',
    'SELECT * FROM read_parquet("{url_table_individu}") WHERE REGION = \'24\''
  )
)
plan_peu_de_lignes


columns_concat <- paste(columns_subset, collapse = ", ")
plan_peu_de_colonnes <- dbGetQuery(
  con,
  glue(  
    'EXPLAIN ANALYZE ',
    'SELECT ({columns_concat}) FROM read_parquet("{url_table_individu}")'
  )
)


capture_parquet_need <- function(string){
  capture <- stringr::str_extract(
    string, "in:\\s*[0-9.]+\\s*[A-Za-z]+"
  )
  return(capture)
}


capture_parquet_need(
  plan_peu_de_lignes$explain_value
)
capture_parquet_need(
  plan_peu_de_colonnes$explain_value
)

# conclusion: duckdb optimise d'abord les colonnes puis fait un filtre sur les lignes: comment faire un filtre sur les lignes en amont ? 
# solution: parquet partitionné


