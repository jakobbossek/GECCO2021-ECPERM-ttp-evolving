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
  max_iters_without_improvement = 10000L, max_time, seed = ceiling(runif(1, min = 1, max = 10000)), ...) {
  if (!checkmate::test_file_exists(x, access = "r", extension = "ttp")) {
    re::catf("[run_ttp_algorithm] At the moment only a path can be passed.")
  }
  instance_folder = paste0(dirname(x), "/")
  instance_filename = basename(x)

  command = "java"
  args = c(
    "-cp build/classes/ Driver", instance_folder, instance_filename,
    algorithm, max_iters_without_improvement, max_time, seed
  )
  command_string = BBmisc::collapse(c(command, args), sep = " ")
  res = BBmisc::system3(command = command, args = args, stdout = TRUE, stderr = TRUE)
  output = as.list(as.numeric(strsplit(res$output, split = " ")[[1L]][-1L]))
  names(output) = c("capacity_free", "weight", "profit", "distance", "travel_time", "objective_score", "runtime")
  return(list(
    call = command_string,
    prob = x,
    output = output,
    raw_output = res
  ))
}

#' @title Fitness fun builder
#'
#' @description Builds a function that expects a TTP instance, runs each
#' \code{algorithm_a} and \code{algorithm_b} each \code{n_runs} times
#' with arguments given by \code{args} (see \code{run_ttp_algorithm}) and
#' returns the ratio of the median objective scores.
#'
#' @param algorithm_a [\code{integer(1)}]\cr
#'   First algoithm.
#' @param algorithm_b [\code{integer(1)}]\cr
#'   Second algoithm.
#' @param n_runs [\code{integer(1)}]\cr
#'   Number of independent runs.
#' @param args [\code{named list}]\cr
#'   Futher arguments passed down to \code{algorithm_a} and \code{algorithm_b}
#'   (see docs of \code{run_ttp_algorithm}).
#' @return [\code{function(x, ...)}] Function which expects a path to a
#' TTP instance and returns a single scalar numeric value.
build_fitness_function = function(algorithm_a, algorithm_b, n_runs, args) {
  force(algorithm_a)
  force(algorithm_b)
  force(n_runs)
  force(args)

  run_multiple = function(x, algorithm, n_runs, args) {
    args2 = c(list(x = x, algorithm = algorithm), args)
    runs = sapply(seq_len(n_runs), function(i) {
      do.call(run_ttp_algorithm, args2)$output$objective_score
    })
    # FIXME: mean or median?
    return(median(runs))
  }

  fun = function(x, ...) {
    runs_a = run_multiple(x, algorithm_a, n_runs, args)
    runs_b = run_multiple(x, algorithm_b, n_runs, args)
    # What if we obtain negative values?
    return(runs_a / runs_b)
  }
  return(fun)
}

doUniformItemsMutation = function(x, pm = 0.1, L = 1000, R = 10000) {
  n = nrow(x)
  k = floor(n * pm)
  idx = sample(seq_len(n), size = k, replace = FALSE)
  # column 1:profits, column 2: weights
  x[idx, 1:2] = matrix(sample(L:R, size = 2 * k, replace = TRUE), ncol = 2L)
  return(x)
}

doCorrelatedItemsMutation = function(x, pm = 0.1, L = 10, R = 100, negative_correlation = FALSE) {
  n = nrow(x)
  k = floor(n * pm)
  R10 = R / 10 # for weakly correlated weights
  corr.factor = if (negative_correlation) -1 else 1
  idx = sample(seq_len(n), size = k, replace = FALSE)
  weights.new = sample(L:R, size = k, replace = TRUE)
  profits.new = floor(runif(k, weights.new - R10, weights.new + R10))
  x[idx, 1:2] = cbind(profits.new, weights.new)
  return(x)
}

doCapacityMutation = function(x, dev = 100) {
  wsum = sum(x$items$weight)
  wsum2 = floor(wsum / 2)
  min = wsum2 - dev
  max = wsum2 + dev
  x$capacity = sample(min:max, size = 1L)
  return(x)
}

applyMutationFromCollection = function(x, collection) {
  mutators = collection$mutators
  n.mutators = length(mutators)
  names = names(mutators)
  probs = if (is.null(collection$probs)) {
    rep(1 / n.mutators, n.mutators)
  }
  idx = sample(seq_len(n.mutators), size = 1L, prob = probs)
  mutator.fun = names[idx]
  mutator.pars = mutators[[mutator.fun]]
  mutator.pars = BBmisc::insert(list(x), mutator.pars)
  y = do.call(mutator.fun, mutator.pars)
  attr(y, "df") = NULL
  return(y)
}

build_mutation = function(...) {
  types = list(...)
  print(types)
  force(types)

  fun = function(x, ...) {
    checkmate::assertClass(x, "ttp_instance")
    if (!is.null(types$kp)) {
      if (runif(1L) < types$tsp$p) {
        x$items[, 1:2] = applyMutationFromCollection(as.matrix(x$items[, 1:2]), types$kp$collection)
      }
    }
    if (!is.null(types$tsp)) {
      if (runif(1L) < types$tsp$p) {
        x$coordinates = applyMutationFromCollection(as.matrix(x$coordinates), types$tsp$collection)
      }
    }
    if (!is.null(types$meta)) {
      if (runif(1L) < types$meta$p) {
        x = applyMutationFromCollection(x, types$meta$collection)
      }
    }
    return(x)
  }
  ecr::makeMutator(mutator = fun, supported = "custom")
}
