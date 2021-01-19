library(tspgen)

devtools::load_all("~/repos/software/r/ttp")
source("r-src/defs.R")
source("r-src/ttp-algorithms.R")
source("r-src/EAs.R")
source("r-src/mutation-operators.R")
source("r-src/fitness-functions.R")

prob = "9competitionInstances/a280_n279_bounded-strongly-corr_01.ttp"

x = generate_random_ttp_instance(n = 100, ipn = 10)


feats = calculate_ttp_features(x)

stop()

TTP::writeProblem(x, path = "test_100_990.ttp")
# res = run_ttp_algorithm(prob, algorithm = 21,
# max_iters_without_improvement = 10000000L,
# max_time = 15L)
# stop("DONE")

# opar = par(mfrow = c(1, 2))
# plot(x$coordinates)
# plot(x$items[, 2:1])
# #stop("DONE")
# par(opar)

# RUN EA
# ===

args = list(max_iters_without_improvement = 1000, max_time = 3)
solver_args = list(max_iters_without_improvement = MAX_ITERS_WITHOUT_IMPROVEMENT, max_time = MAX_TIME_FOR_EACH_SOLVER_RUN)

coll_kp = tspgen::init(preset = "sophisticated")
coll_tsp = tspgen::init(preset = "sophisticated")
coll_meta = tspgen::init()
coll_meta = tspgen::addMutator(coll_meta, "doRentingRateMutation")

mutator_fun = build_mutation(meta = list(p = 1.0, collection = coll_meta), kp = list(p = 1.0, collection = coll_kp), tsp = list(p = 1.0, collection = coll_tsp))

ea_res = EA_generalized(
  ALL_ALGORITHMS,
  args = args,
  type = "pairwise",
  ranking = c(2, 1),
  n = INSTANCE_SIZES,
  ipn = IPN[1],
  max_time = WALLTIME,# - (60 * 60), # one our buffer
  mutator_fun = mutator_fun)
TTP::writeProblem(ea_res$x, path = "evolved.ttp")
eval_res = run_ttp_algorithms_for_evaluation("evolved.ttp", algorithms = ALL_ALGORITHMS, n_runs = 10L, args = args)

boxplot(objective_score ~ algorithm, data = eval_res)
stop("DONE")



# set.seed(1)
# res = run_ttp_algorithm(prob, algorithm = 24,
# max_iters_without_improvement = 1000L,
# max_time = 15L)

# res = run_ttp_algorithms_for_evaluation(x = prob, algorithms = c(23, 24), n_runs = 5, args = list(max_iters_without_improvement = 1000, max_time = 10))
# stop("DONE")

# args = list(
#   max_iters_without_improvement = 1000L,
#   max_time = 5L
# )

# f = build_fitness_function(algorithm_a = 22, algorithm_b = 21,
#   n_runs = 3L, args = args)

# res = f(prob)

# stop("DONE")

coll.kp = tspgen::init()
#coll.kp = tspgen::addMutator(coll.kp, "doUniformItemsMutation", pm = 0.05, L = 10, R = 4000)
coll.kp = tspgen::addMutator(coll.kp, "doExplosionMutation", pm = 0.3)

coll.tsp = tspgen::init()
coll.tsp = tspgen::addMutator(coll.tsp, "doExplosionMutation")
coll.tsp = tspgen::addMutator(coll.tsp, "doClusterMutation")

coll.meta = tspgen::init()
coll.meta = tspgen::addMutator(coll.meta, "doCapacityMutation")

mut.fun = build_mutation(meta = list(p = 1.0, collection = coll.meta), kp = list(p = 1.0, collection = coll.kp), tsp = list(p = 1.0, collection = coll.tsp))

x = TTP::loadProblem(prob)

opar = par(mfrow = c(1, 3))
plot(x$coordinates)
res = downScale(x$coordinates, TRUE)
plot(res$x)
y = upScale(res$x, res$ranges)
plot(y)
par(opar)

#stop("DONE")

x$coordinates = netgen:::rescaleNetworkGlobal2(x$coordinates) # FIXME: export in netgen
x$items[, 1:2] = netgen:::rescaleNetworkGlobal2(as.matrix(x$items[, 1:2]))
y = x
for (i in 1:100) {
  y = mut.fun(y)
}

opar = par(mfrow = c(2, 2))
plot(x$coordinates)
plot(x$items[, 2:1])
plot(y$coordinates)
plot(y$items[, 2:1])
par(opar)
stop("DONE")

# y$items = applyMutationFromCollection(y$items, collection)
# x$items = doUniformItemsMutation(x$items, min = 1e6, max =1e7)


