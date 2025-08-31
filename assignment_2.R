# Function to classify genes
classify_gene <- function(logFC, padj) {
  padj <- ifelse(is.na(padj), 1, padj)
  if (logFC > 1 && padj < 0.05) {
    "Upregulated"
  } else if (logFC < -1 && padj < 0.05) {
    "Downregulated"
  } else {
    "Not_Significant"
  }
}

input_dir  <- "Raw_Data"
output_dir <- "Results"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")
results_list <- lapply(files_to_process, function(file) {
  
  cat("\nProcessing:", file, "\n")
  
  df <- read.csv(file.path(input_dir, file), header = TRUE)
  
  names(df) <- tolower(names(df))
  
  df$padj[is.na(df$padj)] <- 1
  
  df$status <- apply(df, 1, function(row) {
    classify_gene(as.numeric(row["logfc"]), as.numeric(row["padj"]))
  })
  
  out_file <- file.path(output_dir, paste0("Processed_", file))
  write.csv(df, out_file, row.names = FALSE)
  cat("Saved ->", out_file, "\n")
  
  cat("Summary (status counts):\n")
  print(table(df$status))
  
  return(df)
})

names(results_list) <- files_to_process

results_1 <- results_list[["DEGs_Data_1.csv"]]
results_2 <- results_list[["DEGs_Data_2.csv"]]

cat("\nAll done. Check the 'Results' folder and summaries above.\n")
