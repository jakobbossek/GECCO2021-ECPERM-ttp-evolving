library(tidyverse)
library(batchtools)

source("r-src/defs.R")

# COLLECT EVALUATIONS
# ===

for (type in c("trace", "evaluations", "features")) {
  BBmisc::catf("Calculating %s ...", type)
  files = list.files(file.path(OUTPUT_PATH, type), pattern = ".csv$", full.names = TRUE)
  #print(files)
  tbl = do.call(rbind, lapply(files, function(f) {
    tmp = read.table(f, header = TRUE)
    tmp$job.id = gsub(".csv", "", basename(f))
    return(tmp)
  }))
  BBmisc::catf("Writing to %s", file.path(OUTPUT_PATH, sprintf("%s.csv", type)))
  write.table(tbl, file = file.path(OUTPUT_PATH, sprintf("%s.csv", type)), row.names = FALSE, quote = TRUE)
}

stop("DONE")

# COLLECT JOB TABLE
# ===
#if (is.null(reg))
reg = loadRegistry("evolving-registry", writeable = FALSE)
job_tbl = unwrap(getJobTable()[, c("job.id", "algo.pars")])
write.table(job_tbl, file = file.path(OUTPUT_PATH, "job_table.csv"), row.names = FALSE, quote = TRUE)

