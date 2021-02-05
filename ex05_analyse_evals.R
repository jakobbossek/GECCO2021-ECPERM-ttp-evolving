library(tidyverse)
library(ggplot2)
library(ggfortify)

source("r-src/defs.R")
source("r-src/analysis-utils.R")

output_dir = "images/preliminary"

# IMPORT DATA
# ===

jobtbl = readr::read_delim("data/job_table.csv", delim = " ") %>%
  dplyr::mutate(pi = decode_ranking(ranking, type))

evals = readr::read_delim("data/evaluations.csv", delim = " ")
evals = dplyr::left_join(evals, jobtbl, by = "job.id")

feats = readr::read_delim("data/features.csv", delim = " ")
feats = dplyr::left_join(feats, jobtbl, by = "job.id")
types = unique(evals$type)


evals = evals %>%
  filter(!is.na(objective_score)) %>% # FOR NOW SINCE THERE WAS A BUG IN THE FINAL EVALUATIONS
  ungroup()

evalsaggr = evals %>%
  group_by(job.id, instance_size, ipn, ranking, type, pi, algorithm) %>%
  dplyr::summarize(obj_median = median(objective_score, na.rm = TRUE)) %>% #, obj_mean = mean(objective_score)) %>%
  ungroup()

# to wide format (algorithm stat)
evalsaggrwide = reshape2::dcast(evalsaggr, job.id + instance_size + ipn + ranking + type + pi ~ algorithm, value.var = "obj_median")

evalsaggrwide_order = filter(evalsaggrwide, type != "no-order")
evalsaggrwide_noorder = filter(evalsaggrwide, type == "no-order")

evalsaggrwide_order$satisfied = check_ranking_order(evalsaggrwide_order$ranking, as.matrix(evalsaggrwide_order[, c("21", "23", "26")]))

success_overview_order = evalsaggrwide_order %>%
  group_by(type, pi) %>%
  dplyr::summarize(n_success = sum(satisfied), perc_success = 100 * mean(satisfied)) %>%
  ungroup() %>%
  arrange(desc(type), pi)

g = ggplot(success_overview_order, aes(x = as.factor(pi), y = perc_success))
g = g + geom_bar(stat = "identity")
g = g + theme_bw()
g = g + scale_x_discrete(drop = TRUE)
#g = g + scale_y_continuous(labels = scales::percent)
g = g + theme(axis.text.x = element_text(hjust = 1, angle = 25))
g = g + labs(x = "Ranking", y = "Success [in percent]")
g = g + facet_grid(.~type, scales = "free_x")
g
ggsave(file.path(output_dir, "barplot_order_ranking_achieved.pdf"), width = 8, height = 4)

# Now determine actual ranking for the


evalsaggrwide_actual_ranking = evalsaggrwide
evalsaggrwide_actual_ranking$pi = determine_ranking_noorder(as.matrix(evalsaggrwide[, c("21", "23", "26")]))

evalsaggrwide_actual_ranking_aggr = evalsaggrwide_actual_ranking %>%
  group_by(type, pi) %>%
  dplyr::summarize(n = n()) %>%
  ungroup()

feats_with_actual_ranking = dplyr::left_join(feats, evalsaggrwide_actual_ranking, by = "job.id")
library(ggbiplot)

# extract non-NA features and non-constant features
feats_only = feats[, 1:454]
nonna_column_idx = apply(feats_only, 2L, function(x) all(!is.na(x)))
feats_only = feats_only[, nonna_column_idx]
const_feat_column_ids = which(apply(feats_only, 2L, function(x) !all(x == x[1])))
feats_only = feats_only[, const_feat_column_ids]

plot_pca = function(pca_res, tbl) {
  g = autoplot(pca_res, data = tbl, colour = "pi.y", shape = "pi.y")#,
    # loadings = TRUE, loadings.colour = 'blue',
    #        loadings.label = TRUE)#, frame = TRUE)
  g = g + theme_bw()
  g = g + labs(shape = "Ranking", colour = "Ranking")
  g = g + theme(legend.position = "top")
  g = g + scale_color_brewer(palette = "Dark2")
  g = g + scale_shape_manual(values = c(0, 1, 2, 3, 4, 9))
  g
}

plot_tsne = function(tsne_res, tbl) {
  ggtbl = as.data.frame(tsne_res$Y)
  colnames(ggtbl) = c("x1", "x2")
  ggtbl$pi = tbl$pi.y
  print(ggtbl)
  g = ggplot(ggtbl, aes(x = x1, y = x2, colour = as.factor(pi), shape = as.factor(pi)))#,
  g = g + geom_point()
  g = g + theme_bw()
  g = g + labs(shape = "Ranking", colour = "Ranking")
  g = g + theme(legend.position = "top")
  g = g + scale_color_brewer(palette = "Dark2")
  g = g + scale_shape_manual(values = c(0, 1, 2, 3, 4, 9))
  g
}

library(Rtsne)

pca_all_feats = prcomp(feats_only, center = TRUE, scale = TRUE)
pca_tsp_feats = prcomp(feats_only[, grepl("^tsp", colnames(feats_only))], center = TRUE, scale = TRUE)
pca_kp_feats = prcomp(feats_only[, grepl("^knapsack", colnames(feats_only))], center = TRUE, scale = TRUE)

set.seed(123)
tsne_all_feats = Rtsne(feats_only, pca = TRUE)
tsne_tsp_feats = Rtsne(feats_only[, grepl("^tsp", colnames(feats_only))], pca = TRUE)
tsne_kp_feats = Rtsne(feats_only[, grepl("^knapsack", colnames(feats_only))], pca = TRUE)

g = plot_pca(pca_all_feats, feats_with_actual_ranking)
ggsave(file.path(output_dir, "pca-all.pdf"), width = 5.1, height = 5.25)
g = plot_pca(pca_tsp_feats, feats_with_actual_ranking)
ggsave(file.path(output_dir, "pca-tsp.pdf"), width = 5.1, height = 5.25)
g = plot_pca(pca_kp_feats, feats_with_actual_ranking)
ggsave(file.path(output_dir, "pca-kp.pdf"), width = 5.1, height = 5.25)

g = plot_tsne(tsne_all_feats, feats_with_actual_ranking)
ggsave(file.path(output_dir, "tsne-all.pdf"), width = 5.1, height = 5.25)
g = plot_tsne(tsne_tsp_feats, feats_with_actual_ranking)
ggsave(file.path(output_dir, "tsne-tsp.pdf"), width = 5.1, height = 5.25)
g = plot_tsne(tsne_kp_feats, feats_with_actual_ranking)
ggsave(file.path(output_dir, "tsne-kp.pdf"), width = 5.1, height = 5.25)

g = ggplot(evalsaggrwide_actual_ranking_aggr, aes(x = as.factor(pi), y = n, fill = as.factor(type)))
g = g + geom_hline(yintercept = 40, linetype = "dashed")
g = g + geom_bar(stat = "identity", position = "dodge")#, alpha = 0.5)
g = g + theme_bw()
g = g + scale_fill_brewer(palette = "Dark2")
g = g + scale_y_continuous(breaks = c(0, 20, 40, 60, 80, 100, 120, 130))
g = g + theme(legend.position = "top", axis.text.x = element_text(hjust = 1, angle = 25))
g = g + labs(x = "Ranking", y = "Number of instances", fill = "Fitness fun")
#g = g + facet_grid(.~type, scales = "free_x")
g
ggsave(file.path(output_dir, "barplot_actual_order.pdf"), width = 8, height = 5)




# SAMPLE EACH ONE INSTANCE THAT ACHIEVES THE DESIRED RANKING
jobs = evalsaggrwide_order %>%
  group_by(type, pi) %>%
  filter(satisfied == TRUE) %>%
  dplyr::summarize(job.id = job.id[1L]) %>%
  ungroup() %>%
  filter(type == "explicit-ranking")

jobs = BBmisc::convertRowsToList(jobs, name.vector = TRUE)

for (i in 1:length(jobs)) {
  if (i == 3L)
    next
  setup = jobs[[i]]
  print(setup)
  x = TTP::loadProblem(sprintf("data/evolved/%i.ttp", setup$job.id))
  pdf(file.path(output_dir, sprintf("ttp-%s.pdf", gsub(" > ", "-", setup$pi))), width = 8, height = 3.5)
  g = plot_ttp(x)
  dev.off()
}

idxs = list(
  "pairwise" = c(502,533,534,429,407,7,581,583),
  "no-order" = c(1629, 1632, 1682, 1777,1577,1132,732,933),
  "explicit-ranking" = c(1758, 1760, 906, 904, 706, 659, 605, 658))

for (TYPE in types) {
  job.ids = idxs[[TYPE]]
  rename = seq_len(length(job.ids))
  names(rename) = as.character(job.ids)
  evals_final = filter(evals, job.id %in% job.ids) %>%
    mutate(job.id = rename[as.character(job.id)]) %>%
    arrange(job.id) %>%
    mutate(name = sprintf("#%i / %s", job.id, pi))
  evals_final$name = as.factor(evals_final$name)
  print(evals_final)
  g = ggplot(evals_final, aes(x = as.factor(algorithm), y = objective_score))
  g = g + geom_boxplot()
  g = g + geom_jitter(color = "#7570B3", alpha = 0.5, shape = 2)
  g = g + scale_x_discrete(labels = c("S2", "S4", "C2"))
  g = g + theme_bw()
  g = g + scale_y_continuous(labels = label_scientific())
  g = g + facet_wrap(type ~ name, scales = "free_y", ncol=8)
  g = g + labs(x = "Algorithm", y = "Performance")
  #print(g)
  ggsave(file.path(output_dir, sprintf("boxplots_evals_%s.pdf", TYPE)), width = 16, height = 2.4, limitsize = FALSE)
}



for (TYPE in types) {
  g = ggplot(filter(evals, type == TYPE), aes(x = as.factor(algorithm), y = objective_score))
  g = g + geom_boxplot()
  g = g + geom_jitter(color = "#1B9E77", alpha = 0.7, shape = 5)
  g = g + theme_bw()
  g = g + facet_wrap(job.id + ipn ~ pi, scales = "free_y", ncol = 5, labeller = label_both)
  ggsave(file.path(output_dir, sprintf("evals_%s.pdf", TYPE)), width = 18, height =  60, limitsize = FALSE)
}
