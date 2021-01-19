library(tidyverse)
library(batchtools)

source("r-src/defs.R")

# COLLECT EVALUATIONS
# ===
eval_files = list.files(file.path(OUTPUT_PATH, "evaluations"), pattern = ".csv$", full.names = TRUE)
evals_tbl = do.call(rbind, lapply(eval_files, function(f) {
  tbl = read.table(f, header = TRUE)
  tbl$job.id = gsub(".csv", "", basename(f))
  return(tbl)
}))

write.table(evals_tbl, file = file.path(OUTPUT_PATH, "evaluations.csv"), row.names = FALSE, quote = TRUE)

# COLLECT FEATURES
eval_files = list.files(file.path(OUTPUT_PATH, "features"), pattern = ".csv$", full.names = TRUE)
evals_tbl = do.call(rbind, lapply(eval_files, function(f) {
  tbl = read.table(f, header = TRUE)
  tbl$job.id = gsub(".csv", "", basename(f))
  return(tbl)
}))

write.table(evals_tbl, file = file.path(OUTPUT_PATH, "features.csv"), row.names = FALSE, quote = TRUE)

# COLLECT JOB TABLE
# ===
reg = loadRegistry("evolving-registry", writeable = FALSE)
job_tbl = unwrap(getJobTable()[, c("job.id", "algo.pars")])
write.table(job_tbl, file = file.path(OUTPUT_PATH, "job_table.csv"), row.names = FALSE, quote = TRUE)

