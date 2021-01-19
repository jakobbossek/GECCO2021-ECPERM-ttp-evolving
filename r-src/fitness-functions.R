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
build_fitness_function_pairwise = function(algorithm_a, algorithm_b, n_runs, args) {
  force(algorithm_a)
  force(algorithm_b)
  force(n_runs)
  force(args)

  fun = function(x, ...) {
    aggr_a = run_ttp_algorithm_multiple_and_aggregate(x, algorithm_a, n_runs, args)
    aggr_b = run_ttp_algorithm_multiple_and_aggregate(x, algorithm_b, n_runs, args)
    # What if we obtain negative values?
    return(aggr_a / aggr_b)
  }
  return(fun)
}

build_fitness_function_generalized = function(algorithms, type, ranking, n_runs, args) {
  checkmate::assert_choice(type, choices = c("pairwise", "gap-to-second-best", "no-order", "explicit-ranking"))
  force(algorithms)
  force(type)
  force(n_runs)
  force(args)


  fun = function(x, ...) {
    perfs = sapply(algorithms, function(algorithm) {
      run_ttp_algorithm_multiple_and_aggregate(x, algorithm, n_runs, args)
    })

    get_performance_distance = function(p1, p2) {
      p1 - p2
    }

    # handles issues
    if (any(is.na(perfs))) {
      return(NA)
    }

    n = length(perfs)
    #print(perfs)
    if (type == "pairwise") {
      return(get_performance_distance(perfs[ranking[1L]], perfs[ranking[2L]]))
    } else if (type == "gap-to-second-best") {
      # MAXIMIZE the difference between the first algorithm and the second best
      return(get_performance_distance(perfs[ranking[1L]], max(perfs[-ranking[1L]])))
    } else if (type == "no-order") {
      perfs_sorted = sort(perfs)
      p = 0
      for (i in 2:(n-1)) {
        # here ps[1] <= ps[2] <= ... <= ps[n]
        p = p + (perfs_sorted[i] - perfs_sorted[i-1]) * (perfs_sorted[i+1] - perfs_sorted[i])
      }
      return(p)
    } else if (type == "explicit-ranking") {
      bad = 0
      p = 0
      for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
          # i should be better than j
          perf_dist = get_performance_distance(perfs[ranking[i]], perfs[ranking[j]])
          #catf("%i %i: %.3f", i, j, perf_dist)
          if (perf_dist < 0) {
            bad = bad + 1
          } else {
            p = p + perf_dist
          }
        }
      }
      # bad to be minimized, p to be maximized
      return(c(bad, p))
    }
  }
  return(fun)
}

downScale = function(x, memorize = FALSE) {
  ranges = apply(x, 2, range)
  x[, 1] = (x[, 1] - ranges[1, 1])
  x[, 2] = (x[, 2] - ranges[1, 2])
  scale = max(ranges[2, ] - ranges[1, ])
  if (memorize)
    return(list(x = x / scale, ranges = ranges))
  return(x / scale)
}

upScale = function(x, ranges) {
  scale = max(ranges[2, ] - ranges[1, ])
  x = x * scale
  x[, 1L] = x[, 1L] + ranges[1, 1]
  x[, 2L] = x[, 2L] + ranges[1, 2]
  return(x)
}
