library(stringr)
library(DBI)
library(duckdb)

# COMPRENDRE AVEC EXPLAIN ANALYZE --------------

url_table_individu <- "https://minio.lab.sspcloud.fr/projet-formation/bonnes-pratiques/data/RPindividus.parquet"

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
plan_base_complete

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



plan_peu_de_colonnes



plan_base_complete
plan_peu_de_lignes
plan_peu_de_colonnes

# conclusion: duckdb optimise d'abord les colonnes puis fait un filtre sur les lignes: comment faire un filtre sur les lignes en amont ? 
# solution: parquet partitionné