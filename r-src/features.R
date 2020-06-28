calculateFeatures = function(files) {
  future::plan("multicore", workers = future::availableCores() - 1L)
  features = future.apply::future_lapply(files, function(f) {
    x = salesperson::importFromTSPlibFormat(f, round.distances = TRUE, read.opt = TRUE)
    x = salesperson::rescaleNetwork(x)
    #x$coordinates = x$coordinates[!duplicated(x$coordinates)]
    features = salesperson::getFeatureSet(x, black.list = c("Angle", "BoundingBox", "Centroid", "Cluster", "ConvexHull", "Distance", "Modes", "VRP"))
    features$prob = f
    features$n = salesperson::getNumberOfNodes(x)
    features = unlist(features)
    return(features)
  })

  features = as.data.frame(do.call(rbind, features))
  return(features)
}
