library(batchtools)
library(tspgen)

source("r-src/defs.R")

if (!dir.exists(OUTPUT_PATH))
  dir.create(OUTPUT_PATH, recursive = TRUE)

unlink("evaluation-registry", recursive = TRUE)
reg = batchtools::makeRegistry(
  file.dir = "evaluation-registry",
  seed = 1,
  packages = c("netgen", "TTP"),
  source = c("r-src/defs.R", "r-src/ttp-algorithms.R", "r-src/fitness-functions.R", "r-src/mutation-operators.R", "r-src/EAs.R"))

evaluator = function(path_to_instance) {
  tmpdir = "../../../../dev/shm/bossek-ttp/"
  if (!dir.exists(tmpdir)) {
    dir.create(tmpdir)
  }
  on.exit(unlink(tmpdir))

  solver_args = list(max_iters_without_improvement = MAX_ITERS_WITHOUT_IMPROVEMENT, max_time = MAX_TIME_FOR_EACH_SOLVER_RUN)

  tmp_ttp_file = basename(tempfile("tempttp", tmpdir = tmpdir, fileext = ".ttp"))
  x = TTP::loadProblem(path_to_instance)
  TTP::writeProblem(x, path = tmp_ttp_file)

  # final evaluation
  job.id = as.integer(gsub(".ttp", "", basename(path_to_instance)))
  fn = file.path(OUTPUT_PATH, "evaluations", sprintf("%i.csv", job.id))
  if (!dir.exists(dirname(fn)))
    dir.create(dirname(fn), recursive = TRUE)

  eval_res = run_ttp_algorithms_for_evaluation(tmp_ttp_file, ALL_ALGORITHMS, N_RUNS_FOR_EVALUATION, solver_args)
  write.table(eval_res, file = fn, row.names = FALSE)

  return(NA)
}

instances = list.files("/scratch/tmp/bossek/ttp_evolve/data/evolved", pattern = ".ttp$", full.names = TRUE)

# WHY THE FUCK DOES CHUNK NOT WORK HERE WITH FUCKING DATA.TABLE!?!
ids = batchtools::batchMap(evaluator, instances)
class(ids) = "data.frame"
chunk.size = 15
chunks = chunk(ids$job.id, chunk.size = chunk.size)
ids$chunk = do.call(c, lapply(1:length(chunks), function(i) {
  rep(i, length(chunks[[i]]))
}))
class(ids) = c("data.table", "data.frame")

BBmisc::pause()
submitJobs(ids, resources = list(mem = 4000, walltime = 60 * 60))
