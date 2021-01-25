library(batchtools)

logs = list.files("evolving-registry/logs", pattern = "log$", full.names = TRUE)

for (lf in logs) {
  f = readLines(lf)
  f = f[grepl("Best:", f)]
  rts = as.numeric(sapply(strsplit(f, " "), function(l) l[7L]))
  n = length(rts)

  rts_mean = if (length(rts) <= 1) NA else mean(rts[2:n]-rts[1:(n-1)])

  BBmisc::catf("Iters: %i, Mean runtime: %.2f [%s]", n, rts_mean, basename(lf))
  #BBmisc::catf("%s", BBmisc::collapse(rts, sep = " "))
}
