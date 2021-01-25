library(tidyverse)
library(ggplot2)

source("r-src/defs.R")

output_dir = "images/preliminary"

# IMPORT DATA
# ===

jobtbl = readr::read_delim("data/job_table.csv", delim = " ")

traces = readr::read_delim("data/trace.csv", delim = " ")
traces = dplyr::left_join(traces, jobtbl, by = "job.id")

evals = readr::read_delim("data/evaluations.csv", delim = " ")
evals = dplyr::left_join(evals, jobtbl, by = "job.id")

# TIDY: NICE PRESENTATION OF RANKINGS
# ===
decode_ranking = function(xs, types, methods) {
  idlist = strsplit(xs,  split = "-")
  sapply(1:length(idlist), function(i) {
    ids = as.integer(idlist[[i]])
    type = types[i]
    method = methods[i]
    algos = paste0("A", ALL_ALGORITHMS[ids])
    if (method == "pairwise" && type == "pairwise") {
      return(BBmisc::collapse(algos, sep = " > "))
    } else if (method == "generalized" & type == "gap-to-second-best") {
      return(paste0(algos[1L], " > 2nd best"))
    } else if (method == "generalized" & type == "no-order") {
      return("no-order")
    } else if (method == "generalized" & type == "explicit-ranking") {
      return(BBmisc::collapse(algos, sep = " > "))
    } else {
      BBmisc::stopf("[decode_ranking] This case (%s, %s) should not occur!", type, method)
    }
  })
}

# test
#rnks = decode_ranking(c("1-2-3", "1-2", "1-2-3", "3-1-2"), methods = c("generalized", "pairwise", "generalized", "generalized"), types  = c("explicit-ranking", "pairwise", "no-order", "gap-to-second-best"))


evals = evals %>%
  filter(!is.na(objective_score)) %>% # FOR NOW SINCE THERE WAS A BUG IN THE FINAL EVALUATIONS
  dplyr::mutate(pi = decode_ranking(ranking, type, method)) %>%
  ungroup()

set.seed(123)
g = ggplot(filter(evals, job.id %in% sample(unique(evals$job.id), size = 50, replace = FALSE)), aes(x = as.factor(algorithm), y = objective_score))
g = g + geom_boxplot()
g = g + theme_bw()
g = g + facet_wrap(job.id ~ pi, scales = "free_y", ncol = 5, labeller = label_both)
print(g)
ggsave(file.path(output_dir, "evals_sampled.pdf"), width = 14, height =  13)

stop()

# TRAJECTORIES
# ===

g = ggplot(filter(traces, iter %% 100 == 0, job.id %in% sample(unique(traces$job.id), size = 10, replace = FALSE)), aes(x = iter, y = fitness))
g = g + geom_line() + geom_point(size = 0.3)
g = g + theme_bw()
g = g + theme(axis.text.x = element_text(angle = 45, hjust = 1))
g = g + facet_wrap(. ~ job.id, scales = "free_y", ncol = 5, labeller = label_both)
print(g)

ggsave(file.path(output_dir, "traces.pdf"), plot = g, width = 14, height = 5)


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
