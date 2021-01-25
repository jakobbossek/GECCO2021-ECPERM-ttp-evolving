library(ggplot2)
library(tidyverse)

rts = readLines("runtimes.csv")
k = 0
rts = do.call(rbind, lapply(strsplit(rts, " "), function(x) {
  k <<- k + 1L
  x = as.numeric(x)
  n = length(x)
  data.frame(iteration = seq_len(n-1), runtime = x[2:n] - x[1:(n-1)], job = k)
}))

g = ggplot(rts, aes(x = iteration, y = runtime))
g = g + geom_line() + geom_point(size = 0.1)
g = g + facet_wrap(.~job, labeller = label_both, ncol = 10)
g = g + theme_bw()
#print(g)
ggsave("images/issues/individual_runtimes_by_iteration.pdf", plot = g, width = 10, height = 15)


rtsaggr = rts %>%
  group_by(job) %>%
  dplyr::summarize(n = length(runtime), runtime = mean(runtime)) %>%
  ungroup()

g = ggplot(rtsaggr, aes(x = n, y = runtime)) + geom_line() + geom_point() + theme_bw()
g = g + labs(x = "Nr. of iterations", y = "Mean runtime")
print(g)
ggsave("images/issues/mean_runtime_by_iterations.pdf", plot = g, width = 8, height = 3)
