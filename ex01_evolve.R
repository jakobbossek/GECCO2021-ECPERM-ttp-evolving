library(batchtools)
library(tspgen)

source("r-src/defs.R")

if (!dir.exists(OUTPUT_PATH))
  dir.create(OUTPUT_PATH, recursive = TRUE)

unlink("evolving-registry", recursive = TRUE)
reg = batchtools::makeExperimentRegistry(
  file.dir = "evolving-registry",
  seed = 1,
  packages = c("tspgen", "ecr", "netgen", "TTP", "batchtools"),
  source = c("r-src/defs.R", "r-src/ttp-algorithms.R", "r-src/fitness-functions.R", "r-src/mutation-operators.R", "r-src/EAs.R"))

problem = batchtools::addProblem("DUMMY", data = list())

batchtools::addAlgorithm("EA", fun = function(job, data, ...) {
  args = list(...)

  solver_args = list(max_iters_without_improvement = MAX_ITERS_WITHOUT_IMPROVEMENT, max_time = MAX_TIME_FOR_EACH_SOLVER_RUN)

  coll_kp = tspgen::init(preset = "sophisticated")
  coll_tsp = tspgen::init(preset = "sophisticated")
  coll_meta = tspgen::init()
  coll_meta = tspgen::addMutator(coll_meta, "doRentingRateMutation")

  mutator_fun = build_mutation(meta = list(p = 1.0, collection = coll_meta), kp = list(p = 1.0, collection = coll_kp), tsp = list(p = 1.0, collection = coll_tsp))
  ranking = as.integer(strsplit(args$ranking, split = "-")[[1]])
  #print(ranking)

  fitness_fun = build_fitness_function_generalized(ALL_ALGORITHMS, args$type, ranking, n_runs = N_RUNS_DURING_EVOLUTION, solver_args)

  ea_res = EA_generalized(
    fitness_fun = fitness_fun,
    type = args$type,
    n = args$instance_size,
    ipn = args$ipn,
    max_time = WALLTIME,# - (60 * 60), # one our buffer
    mutator_fun = mutator_fun,
    tmpdir = TMP_DIR)

  # save instance
  fn = file.path(OUTPUT_PATH, "evolved", sprintf("%i.ttp", job$job.id))
  if (!dir.exists(dirname(fn)))
    dir.create(dirname(fn), recursive = TRUE)
  #print(fn)
  TTP::writeProblem(ea_res$x, path = fn)

  # final evaluation
  fn = file.path(OUTPUT_PATH, "evaluations", sprintf("%i.csv", job$job.id))
  if (!dir.exists(dirname(fn)))
    dir.create(dirname(fn), recursive = TRUE)

  if (!dir.exists(TMP_DIR)) {
    dir.create(TMP_DIR)
  }
  tmp_ttp_file = basename(tempfile("tempttp", tmpdir = TMP_DIR, fileext = ".ttp"))
  TTP::writeProblem(ea_res$x, path = tmp_ttp_file)
  eval_res = run_ttp_algorithms_for_evaluation(tmp_ttp_file, ALL_ALGORITHMS, N_RUNS_FOR_EVALUATION, solver_args)
  unlink(tmp_ttp_file)
  write.table(eval_res, file = fn, row.names = FALSE)

  # features
  fn = file.path(OUTPUT_PATH, "features", sprintf("%i.csv", job$job.id))
  if (!dir.exists(dirname(fn)))
    dir.create(dirname(fn), recursive = TRUE)
  feats = as.data.frame(calculate_ttp_features(ea_res$x))
  write.table(feats, file = fn, row.names = FALSE)

  # trace
  fn = file.path(OUTPUT_PATH, "trace", sprintf("%i.csv", job$job.id))
  if (!dir.exists(dirname(fn)))
    dir.create(dirname(fn), recursive = TRUE)
  write.table(ea_res$trace, file = fn, row.names = FALSE)

  return(list(eval_res = eval_res, trace = ea_res$trace))
})

make_rankings = function(n, k = NULL) {
  perms = combinat::permn(seq_len(n))
  if (!is.null(k))
    perms = lapply(perms, function(perm) perm[1:k])
  sapply(perms, BBmisc::collapse, sep = "-")
}

pairwise_rankings = make_rankings(3, 2)
generalized_rankings = make_rankings(3)

EA_design_pairwise = data.table::CJ(
  instance_size = INSTANCE_SIZES, # only 250 for now
  ipn = IPN,
  ranking = pairwise_rankings,
  type = "pairwise")

EA_design_generalized = data.table::CJ(
  instance_size = INSTANCE_SIZES, # only 250 for now
  ipn = IPN,
  ranking = generalized_rankings,
  type = GENERALIZED_FITNESS_TYPES)

algo.designs = list(EA = rbind(EA_design_pairwise, EA_design_generalized))

batchtools::addExperiments(algo.designs = algo.designs, repls = N_INSTANCES)

BBmisc::pause()

#ids = batchtools::findExperiments(repls = seq_len(10))
ids = findNotDone()
#ids = ids[sample(1:nrow(ids), 10), ]
submitJobs(ids, resources = list(mem = 8000, walltime = WALLTIME_ON_NODE))


stop("DONE")
