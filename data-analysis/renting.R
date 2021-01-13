library(tidyverse)

raw = as.numeric(readr::read_delim("data-analysis/renting.csv", delim = ":")[[2L]])
hist(raw, xlab = "Renting Ratio", main = "Histogram of renting ratios of all TTP files")
