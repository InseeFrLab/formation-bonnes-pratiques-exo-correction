columns_subset <- c(
  "REGION", "AGED", "ANAI", "CATL", "COUPLE",
  "SEXE", "SURF", "TP", "TRANS", "IPONDI"
)

filename_sample_csv <- "data/RPindividus_24.csv"
filename_sample_parquet <- gsub("csv", "parquet", filename_sample_csv)
filename_full_parquet <- gsub("_24", "", filename_sample_parquet)
filename_full_csv <- gsub("parquet", "csv", filename_full_parquet)


start_time <- Sys.time()
read_parquet(filename_sample_parquet)
end_time <- Sys.time()
diff_time1 <- end_time - start_time

start_time <- Sys.time()
read_parquet(filename_sample_parquet) %>% select(any_of(columns_subset))
end_time <- Sys.time()
diff_time2 <- end_time - start_time

start_time <- Sys.time()
open_dataset(filename_sample_parquet) %>% collect()
end_time <- Sys.time()
diff_time3 <- end_time - start_time

start_time <- Sys.time()
open_dataset(filename_sample_parquet) %>% select(any_of(columns_subset)) %>% collect()
end_time <- Sys.time()
diff_time4 <- end_time - start_time

start_time <- Sys.time()
readr::read_csv(filename_sample_csv)
end_time <- Sys.time()
diff_time5 <- end_time - start_time

start_time <- Sys.time()
readr::read_csv(filename_sample_csv) %>% select(any_of(columns_subset))
end_time <- Sys.time()
diff_time6 <- end_time - start_time
