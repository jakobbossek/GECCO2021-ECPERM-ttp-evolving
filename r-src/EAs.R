EA_generalized = function(fitness_fun, type, n, ipn, max_time, mutator_fun, tmpdir) {
  #checkmate::assert_integer(algorithms)
  max_time = checkmate::asInt(max_time)

  st = proc.time()
  iter = 0L

  if (!dir.exists(tmpdir))
    dir.create(tmpdir, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE))

  tmp_ttp_file = tempfile("tempttp", tmpdir = tmpdir, fileext = ".ttp")
  BBmisc::catf("Writing temp file to %s", tmp_ttp_file)

  x_fitness = NA
  while (any(is.na(x_fitness))) {
    x = generate_random_ttp_instance(n = n, ipn = ipn)
    if (!dir.exists(tmpdir)) {
      dir.create(tmpdir)
    }
    TTP::writeProblem(x, path = tmp_ttp_file, overwrite = TRUE)
    x_fitness = fitness_fun(tmp_ttp_file)
  }

  time_passed = (proc.time() - st)[3L]
  fittrack = x_fitness
  if (length(x_fitness) == 1L)
    fittrack = c(fittrack, NA, NA)

  trace = data.frame(iter = iter, time_passed = time_passed, fitness1 = fittrack[1], fitness2 = fittrack[2], fitness3 = fittrack[3L])

  while (time_passed < max_time) {
    iter = iter + 1L
    fitness_eval_error = FALSE
    y = x
    # down-scale to [0,1]
    y$coordinates = downScale(y$coordinates)
    y$items[, 1:2] = downScale(y$items[, 1:2])
    # modify
    y = mutator_fun(y)
    # up-scale back

    y$coordinates = round(y$coordinates * 10000)
    y$items$weight = round((y$items$weight * 4040) + 1)
    y$capacity = as.integer(round((runif(1, min = 1, max = 10) / 11) * sum(y$items$weight)))
    y$items$profit = round((y$items$profit * 4400) + 1)

    opar = par(mfrow = c(1, 2))
    plot(y$coordinates)
    plot(y$items[, c("weight", "profit")])
    par(opar)

    stwr = proc.time()
    if (!dir.exists(tmpdir)) {
      dir.create(tmpdir)
    }
    TTP::writeProblem(y, path = tmp_ttp_file, overwrite = TRUE)
    BBmisc::catf("Time to write problem: %.2f", (proc.time() - stwr)[3L])
    # evaluate (tournament to avoid lucky samples)
    y_fitness = fitness_fun(tmp_ttp_file)
    #x_fitness = fitness_fun(x)

    is_lexicographically_larger = function(x, y) {
      n = length(x)
      for (i in seq_len(n)) {
        if (x[i] > y[i]) {
          return(TRUE)
        } else if (x[i] < y[i]) {
          return (FALSE)
        }
      }
      return (TRUE) # if x[i] == y[i] for all i in [n]
    }

    # print(y_fitness)
    # print(x_fitness)

    # Sometimes algorithms fail or for some reason parsing the results fails (rarely)
    fitness_eval_error = any(is.na(y_fitness))
    if (!fitness_eval_error) {
      if (type != "explicit-ranking") {
        if (y_fitness >= x_fitness) {
          x = y
          x_fitness = y_fitness
        }
      } else {
        # lecicographic order: y lex x <=> y[1] < x[1] or y[1] == x[1] and y[2] > x[2]
         if (is_lexicographically_larger(y_fitness, x_fitness)) {
          x = y
          x_fitness = y_fitness
        }
      }
    }
    time_passed = (proc.time() - st)[3L]
    fittrack = x_fitness
    if (length(x_fitness) == 1L)
      fittrack = c(fittrack, NA, NA)

    trace = rbind(trace, data.frame(iter = iter, time_passed = time_passed, fitness1 = fittrack[1], fitness2 = fittrack[2], fitness3 = fittrack[3L]))
    re::catf("Iter: %i, Best: (%.2f, %.2f, %.2f), time passed: %.2f%s\n", iter, fittrack[1L], fittrack[2L], fittrack[3L], time_passed, ifelse(fitness_eval_error, " [errored]", ""))
  }

  re::catf("EA TERMINATED.\n")

  return(list(x = x, trace = trace))
}

#' Generate random TTP instance.
#'
#' @description Creates a random instance for the traveling thief problem (TTP)
#' with node coordinates and weights/profits sampled uniformly at random within
#' reasonable box-constraints inspired by the very first TTP paper.
#' The knapsack capacity and renting rate are also sampled uniformly at random
#' while the minimal and maximal velocity are set to \eqn{v_{min}=0.1} and \eqn{v_max=1}
#' respectively.
#'
#' @param n [\code{integer(1)}]\cr
#'   Number of nodes.
#' @param ipn [\code{integer(1)}]\cr
#'   Number of \dQuote{items per node}. Note that the total number of items
#'   equals \eqn{(n-1)\cdot ipn} since there are no items to be collected in
#'   the first node.
#' @return [\code{ttp_instance}]
generate_random_ttp_instance = function(n, ipn = 1L, v_min = 0.1, v_max = 1,
  r_bounds = c(0, 1000), c_bounds = c(1, 10),
  weight_bounds = c(1, 4040), profit_bounds = c(1, 4400),
  node_coordinate_bounds = c(0, 10000)) {
  m = (n - 1) * ipn

  # actual instance
  coordinates = as.data.frame(matrix(round(runif(2 * n, min = node_coordinate_bounds[1L], max = node_coordinate_bounds[2L])), ncol = 2L))
  items = data.frame(
    profit = round(runif(m, min = profit_bounds[1L], max = profit_bounds[2L])),
    weight = round(runif(m, min = weight_bounds[1L], max = weight_bounds[2L])),
    nodenr = rep(2:n, each = ipn))

  # meta data
  weight_sum = sum(items$weight)
  C = round(runif(1L, min = c_bounds[1L], max = c_bounds[2L]), digits = 2L)
  capacity = as.integer(round((C / 11) * weight_sum))
  R = round(runif(1L, r_bounds[1], r_bounds[2L]), digits = 2L)

  # wrap up
  res = list(
    name = "random ttp instance",
    type = "evolved",
    n = n,
    m = m,
    capacity = capacity,
    vmin = v_min,
    vmax = v_max,
    R = runif(1L, min = r_bounds[1L], max = r_bounds[2L]),
    edge_weight_type = "CEIL_2D",
    coordinates = coordinates,
    items = items
  )
  class(res) = c("ttp_instance", "tsp_instance")
  return(res)
}
