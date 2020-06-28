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
