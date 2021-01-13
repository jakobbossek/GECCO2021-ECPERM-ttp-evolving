doUniformItemsMutation = function(x, pm = 0.1, L = 1000, R = 10000) {
  n = nrow(x)
  k = floor(n * pm)
  idx = sample(seq_len(n), size = k, replace = FALSE)
  # column 1:profits, column 2: weights
  x[idx, 1:2] = matrix(sample(L:R, size = 2 * k, replace = TRUE), ncol = 2L)
  return(x)
}

# doCorrelatedItemsMutation = function(x, pm = 0.1, L = 10, R = 100, negative_correlation = FALSE) {
#   n = nrow(x)
#   k = floor(n * pm)
#   R10 = R / 10 # for weakly correlated weights
#   corr.factor = if (negative_correlation) -1 else 1
#   idx = sample(seq_len(n), size = k, replace = FALSE)
#   weights.new = sample(L:R, size = k, replace = TRUE)
#   profits.new = floor(runif(k, weights.new - R10, weights.new + R10))
#   x[idx, 1:2] = cbind(profits.new, weights.new)
#   return(x)
# }

doRentingRateMutation = function(x, min = 1, max = 1000) {
  x$R = x$R + rnorm(1, mean = 0, sd = 10)
  x$R = min(max(1, x$R), 1000)
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
      if (runif(1L) < types$kp$p) {
        x$items[, 1:2] = netgen:::rescaleNetworkGlobal2(applyMutationFromCollection(as.matrix(x$items[, 1:2]), types$kp$collection))
      }
    }
    if (!is.null(types$tsp)) {
      if (runif(1L) < types$tsp$p) {
        x$coordinates = netgen:::rescaleNetworkGlobal2(applyMutationFromCollection(as.matrix(x$coordinates), types$tsp$collection))
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
