library(stringr)
library(DBI)
library(duckdb)

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
