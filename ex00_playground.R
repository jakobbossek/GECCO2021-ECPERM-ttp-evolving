source("r-src/ttp-algorithms.R")

prob = "9competitionInstances/a280_n279_bounded-strongly-corr_01.ttp"

set.seed(1)
res = run_ttp_algorithm(prob, algorithm = 24,
max_iters_without_improvement = 100000L,
max_time = 15L)

args = list(
  max_iters_without_improvement = 10000L,
  max_time = 5L
)

f = build_fitness_function(algorithm_a = 22, algorithm_b = 21,
  n_runs = 3L, args = args)

res = f(prob)
