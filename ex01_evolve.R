library(batchtools)
library(tspgen)

source("r-src/defs.R")

if (!dir.exists(OUTPUT_PATH))
  dir.create(OUTPUT_PATH, recursive = TRUE)

unlink("evolving-registry", recursive = TRUE)
reg = batchtools::makeExperimentRegistry(
  file.dir = "evolving-registry",
  seed = 1,
  packages = c("tspgen", "ecr", "netgen", "TTP"),
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
  print(ranking)

  ea_res = EA_generalized(
    ALL_ALGORITHMS,
    args = solver_args,
    type = args$type,
    ranking = ranking,
    n = args$instance_size,
    ipn = args$ipn,
    max_time = WALLTIME,# - (60 * 60), # one our buffer
    mutator_fun = mutator_fun)

  # build file-name from job
  job_pars = unwrap(getJobPars(job$job.id)[, "algo.pars"])
  job_pars$repl = job$repl
  job_string = BBmisc::collapse(job_pars, sep = "_")

  # save instance
  fn = file.path(OUTPUT_PATH, "evolved", sprintf("evolved-%s.ttp", job_string))
  if (!dir.exists(dirname(fn)))
    dir.create(dirname(fn), recursive = TRUE)
  print(fn)
  TTP::writeProblem(ea_res$x, path = fn)

  # final evaluation
  fn = file.path(OUTPUT_PATH, "evaluations", sprintf("%s.csv", job_string))
  if (!dir.exists(dirname(fn)))
    dir.create(dirname(fn), recursive = TRUE)
  eval_res = run_ttp_algorithms_for_evaluation(ea_res$x, ALL_ALGORITHMS, N_RUNS_FOR_EVALUATION, solver_args)
  eval_res = cbind(eval_res, job_pars)
  write.table(eval_res, file = fn, row.names = FALSE)
  return(eval_res)
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
  instance_size = INSTANCE_SIZES, # only 100 for now
  ipn = IPN,
  ranking = pairwise_rankings,
  type = "pairwise",
  method = "pairwise")

EA_design_generalized = data.table::CJ(
  instance_size = INSTANCE_SIZES, # only 100 for now
  ipn = IPN,
  ranking =generalized_rankings,
  type = GENERALIZED_FITNESS_TYPES,
  method = "generalized")



algo.designs = list(EA = rbind(EA_design_pairwise, EA_design_generalized))

batchtools::addExperiments(algo.designs = algo.designs, repls = N_INSTANCES)


