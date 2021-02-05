# java -cp build/classes/ Driver 9competitionInstances/ a280_n279_bounded-strongly-corr_01.ttp 24 10000 10 321
# macht folgendes:

# arg[0]  Verzeichnis wo die instance files sind
# arg[1]  instance file name
# arg[2]  algorithm ID
# arg[3]  number of iterations that algorithms like 1+1EA get before saying “I’ve converged"
# arg[4]  total time in milliseconds
# arg[5]  random number seed (not pushed through to linkern for some reason)

#' @title Run TTP heuristic
#'
#' @description Wrapper around Java code by Markus Wagner to access multiple TTP solving heuristics.
#' The java code is executed via \code{system} rather than explicitly.
#'
#' @param x [\code{character(1)}]\cr
#'   Relative or absolute file path to TTP instance.
#' @param algorithm [\code{integer(1)}]\cr
#'   Algorithm number.
#' @param exec_path [any]\cr
#'   Not used at the moment.
#' @param max_iters_without_improvement [\code{integer(1)}]\cr
#'   Number of iterations that algorithms like (1+1) EA get before saying \dQuote{I've converged}.
#'   Default is 10000.
#' @param max_time [\code{integer(1)}]\cr
#'   Time limit in milliseconds.
#' @param seed [\code{integer(1)}]\cr
#'   Random number seed (not pushed through to linkern for some reason).
#' @return [\code{list}] List with elements:
#' \describe{
#'   \item{call [\code{character(1)}]}{Actual command (with arguments) passed down to \code{system}.}
#'   \item{prob [\code{character(1)}]}{Path to instance, i.e. \code{x}.}
#'   \item{output [\code{named list}]}{Named list with algorithm output.}
#'   \item{raw_output [\code{list}]}{Raw output of \code{\link[BBmisc]{system3}.}
#' }
run_ttp_algorithm = function(x, algorithm, exec_path,
  max_iters_without_improvement = 10000L, max_time, seed = ceiling(runif(1, min = 1, max = 100000)), ...) {

  # temporarily export if loaded instance is passed
  if (checkmate::test_class(x, "ttp_instance")) {
    tf = basename(tempfile("tempttp", tmpdir = getwd(), fileext = ".ttp"))
    #re::catf("[run_ttp_algorithm] Loaded instance passed. Exporting to temporary file %s.\n", basename(tf))
    TTP::writeProblem(x, path = tf, overwrite = TRUE)
    x = tf
    on.exit(unlink(x))
  }

  #BBmisc::catf("Reading file from %s", x)

  if (!checkmate::test_file_exists(x, access = "r", extension = "ttp")) {
    re::catf("[run_ttp_algorithm] File not found.")
  }

  instance_folder = dirname(x)
  # obviously the java implementation does not work with absolute path
  #instance_folder = "./"
  instance_filename = basename(x)
  #TODO: hard-coded! :(
  tmp_dir_linkern = "../../../../dev/shm/bossek-ttp/"
  tmp_dir_linkern = "/Users/bossek/tmp/ttp_evolve"
  command = "java"
  args = c(
    "-cp build/classes/ Driver", instance_folder, instance_filename,
    algorithm, max_iters_without_improvement, max_time, seed,
    tmp_dir_linkern
  )
  command_string = BBmisc::collapse(c(command, args), sep = " ")
  #BBmisc::catf("[Call] %s", command_string)
  done = FALSE
  max_tries = 5L
  n_tries = 0
  res = NULL
  st = proc.time()
  while (!done && (n_tries < max_tries)) {
    n_tries = n_tries + 1
    res = try({BBmisc::system3(command = command, args = args, stdout = TRUE, stderr = TRUE)})
    done = !inherits(res, "try-error")
  }

  output = as.list(rep(NA, 7))
  if (done) {
    output = as.list(as.numeric(strsplit(res$output, split = " ")[[1L]][-1L]))
  }
  if (any(is.na(output)) || !done) {
    output = as.list(rep(NA, 7))
  }
  names(output) = c("capacity_free", "weight", "profit", "distance", "travel_time", "objective_score", "runtime")
  BBmisc::catf("[debug] Run algorithm %i terminated (nt: %i, success: %i)", algorithm, n_tries, as.integer(done))
  BBmisc::catf("Run took %.2f [s] (max time is %.2f [s])", (proc.time() - st)[3L], max_time)

  # delay too fast return
  #Sys.sleep(sample(1:3, 1))

  return(list(
    call = command_string,
    prob = x,
    output = output,
    raw_output = res
  ))
}

#' Run TTP heuristic multiple times.
#'
#' @description An heuristic TTP solver is run multiple times. The aggregated objective
#' value is returned.
#'
#' @inheritParams run_ttp_algorithm
#' @param args [\code{list}]\cr
#'   List of arguments passed down to \code{algorithm}.
#' @param n_runs [\code{integer(1)}]\cr
#'   Number of independent runs.
#' @param aggr.fun [\code{function}]\cr
#'   Function used to aggregate results. Default is \code{\link[stats]{median}}.
#' @return [\code{numeric(1)}]
run_ttp_algorithm_multiple_and_aggregate = function(x, algorithm, n_runs, args, aggr.fun = stats::median) {
  args2 = c(list(x = x, algorithm = algorithm), args)
    runs = sapply(seq_len(n_runs), function(i) {
      do.call(run_ttp_algorithm, args2)$output$objective_score
    })
    return(aggr.fun(runs))
}

#' Run multiple heuristics multiple times.
#'
#' @description This is a helper function for final evaluations. Given a set of algorithms and
#' an instance, each algorithm is run multiple times independently. The function returns a
#' data frame.
run_ttp_algorithms_for_evaluation = function(x, algorithms, n_runs, args) {
  exp_grid = expand.grid(algorithm = algorithms, run = seq_len(n_runs), stringsAsFactors = FALSE)
  exp_grid = re::rowsToList(exp_grid)
  #print(exp_grid)
  results = lapply(exp_grid, function(setup) {
    args2 = c(list(x = x, algorithm = setup$algorithm), args)
    #print(args2)
    tmp = do.call(run_ttp_algorithm, args2)
    tmp = as.data.frame(c(prob = tmp$prob, tmp$output), stringsAsFactors = FALSE)
    tmp$run = setup$run
    tmp$algorithm = setup$algorithm
    return(tmp)
  })
  do.call(rbind, results)
}

calculate_ttp_features = function(x) {
  coord_tsp = salesperson::makeNetwork(coordinates = as.matrix(x$coordinates), name = "tsp", get.distances = TRUE)
  coord_knapsack = salesperson::makeNetwork(coordinates = as.matrix(x$items[, c("weight", "profit")]), name = "knapsack", get.distances = TRUE)

  feats_tsp = salesperson::getFeatureSet(coord_tsp, black.list = "VRP")
  names(feats_tsp) = paste0("tsp_", names(feats_tsp))
  feats_knapsack = salesperson::getFeatureSet(coord_knapsack, black.list = "VRP")
  names(feats_knapsack) = paste0("knapsack_", names(feats_knapsack))
  return(c(feats_tsp, feats_knapsack))
}
