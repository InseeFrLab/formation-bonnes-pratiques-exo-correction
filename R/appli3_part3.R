library(arrow)

columns_subset <- c(
  "REGION", "AGED", "ANAI", "CATL", "COUPLE",
  "SEXE", "SURF", "TP", "TRANS", "IPONDI"
)

filename_sample_csv <- "data/RPindividus_24.csv"
filename_sample_parquet <- gsub("csv", "parquet", filename_sample_csv)
filename_full_parquet <- gsub("_24", "", filename_sample_parquet)
filename_full_csv <- gsub("parquet", "csv", filename_full_parquet)


open_dataset(filename_full_parquet) %>%
  group_by(REGION, DEPT) %>%
  write_dataset("./data/RPindividus")



start_time <- Sys.time()
open_dataset("./data/RPindividus") %>% filter(REGION == 94, DEPT == "2A") %>% collect()
end_time <- Sys.time()
diff_time3 <- end_time - start_time