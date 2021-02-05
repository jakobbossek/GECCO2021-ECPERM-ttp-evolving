library(tidyverse)
library(ggplot2)

source("r-src/defs.R")
source("r-src/analysis-utils.R")
output_dir = "images/preliminary"

# IMPORT DATA
# ===

jobtbl = readr::read_delim("data/job_table.csv", delim = " ") %>%
  dplyr::mutate(pi = decode_ranking(ranking, type))

traces = readr::read_delim("data/trace.csv", delim = " ", col_types = "iddddi")
traces = dplyr::left_join(traces, jobtbl, by = "job.id")

types = unique(jobtbl$type)

# CHECK HOW STUFF LOOKED LIKE IN VERY FIRST ITERATION
# ===
tbl_pairwise = filter(traces, iter == 0, type == "pairwise", grepl("> C2", pi)) %>%
  select(fitness1, pi)

# TRAJECTORIES
# ===

for (TYPE in types) {
  tmptbl = filter(traces, iter %% 1000 == 0, type == TYPE)
  if (TYPE == "explicit-ranking")
    tmptbl = filter(tmptbl, fitness3 > 0)
  tmptbl = reshape2::melt(tmptbl, id.vars = setdiff(colnames(tmptbl), c("fitness1", "fitness2", "fitness3")), variable.name = "fitnessnr", value.name = "fitness")
  print(as_tibble(tmptbl))
  g = ggplot(tmptbl, aes(x = iter, y = fitness, color = fitnessnr))
  g = g + geom_line() + geom_point(size = 0.3)
  g = g + theme_bw()
  g = g + scale_color_brewer(palette = "Dark2")
  g = g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  g = g + facet_wrap(job.id + ipn~ pi, scales = "free_y", ncol = 5, labeller = label_both)
  ggsave(file.path(output_dir, sprintf("traces-%s.pdf", TYPE)), plot = g, width = 14, height = 60, limitsize = FALSE)
}

stop()


# STATISTICS ON THE NUMBER OF FITNESS EVALUATIONS
# ===
aggr = traces %>%
  group_by(job.id, type, method) %>%
  dplyr::summarize(niters = n(), min = min(fitness), max = max(fitness)) %>%
  ungroup()

g = ggplot(aggr, aes(niters, color = type))
g = g + stat_ecdf(geom = "step")
g = g + theme_bw()
print(g)
