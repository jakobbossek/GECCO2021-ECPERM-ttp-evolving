library(tspgen)

devtools::load_all("~/repos/software/r/ttp")
source("r-src/ttp-algorithms.R")

prob = "9competitionInstances/a280_n279_bounded-strongly-corr_01.ttp"

coll.kp = tspgen::init()
#coll.kp = tspgen::addMutator(coll.kp, "doUniformItemsMutation", pm = 0.05, L = 10, R = 4000)
#coll.kp = tspgen::addMutator(coll.kp, "doCorrelatedItemsMutation", pm = 0.05, L = 10, R = 3900)
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

stop("DONE")

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
