library(ggplot2)
library(gridExtra)
devtools::load_all("/Users/bossek/repos/software/r/tspgen")
source("r-src/mutation-operators.R")
source("r-src/EAs.R")
source("r-src/fitness-functions.R")

n = 500L
pm = 0.4
min.eps = 0.2
max.eps = 0.3
jitter.sd = 0.05
p.rot = 0
p.jitter = 0
iters = 1000L

options(tspgen.debug = TRUE)

plot_ttp = function(coords, items) {
  any_mutated = any(x$mutated)

  plot_ttp_internal = function(x) {
    x$Mutated = ifelse(x$mutated, "yes", "no")
    g = ggplot(data = x, aes(x = x1, y = x2))
    g = g + theme_bw()
    if (any(x$mutated)) {
      g = g + geom_point(aes(shape = Mutated, color = Mutated))
      g = g + scale_color_manual(breaks = c("yes", "no"), values = c("blue", "black"))
      g = g + scale_shape_manual(breaks = c("yes", "no"), values = c(3, 1))
      g = g + theme(
        legend.box.background = element_rect(),
        legend.position = c(.97, .97),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      )
    } else {
      g = g + geom_point(shape = 1)
    }
    return(g)
  }
  gcoords = plot_ttp_internal(coords) + xlab("node x-coordinate") + ylab("node y-coordinate") + xlim(c(0, 10000)) + ylim(c(0, 10000))
  gitems  = plot_ttp_internal(items) + xlab("weight") + ylab("profit") + xlim(c(0, 4040)) + ylim(c(0, 4400))
  gridExtra::grid.arrange(gcoords, gitems, nrow = 1L)
}

set.seed(11L)
x = generate_random_ttp_instance(n)
y = x

# data for plot before mutation (initial solution)
x_coords = mutationAsDataframe(x$coordinates, integer(0L))
x_items = mutationAsDataframe(x$items[, 1:2], integer(0L))
#x_coords[, 1:2] = forceToBounds(as.matrix(x_coords[, 1:2]), lower = 0, upper = 10000)
#x_items[, 1:2] = forceToBounds(as.matrix(x_items[, 1:2]), lower = 0, upper = 4400)

# downscale
y$coordinates = downScale(y$coordinates)
y$items[, 1:2] = downScale(y$items[, 1:2])

# mutator
coll_coords = tspgen::init()
coll_coords = tspgen::addMutator(coll_coords, "doExplosionMutation", min.eps = min.eps, max.eps = max.eps)
coll_items = tspgen::init()
coll_items = tspgen::addMutator(coll_items, "doLinearProjectionMutation", pm = pm, min.eps = min.eps, max.eps = max.eps)

y_coords = applyMutationFromCollection(as.matrix(y$coordinates), coll_coords, drop.attr = FALSE)
y_coords = attr(y_coords, "df")
y_coords[, 1:2] = forceToBounds(as.matrix(y_coords[, 1:2]))
y_coords[, 1:2] = y_coords[, 1:2]

y_items = applyMutationFromCollection(as.matrix(y$items[, 1:2]), coll_items, drop.attr = FALSE)
y_items = attr(y_items, "df")
y_items[, 1:2] = forceToBounds(as.matrix(y_items[, 1:2]))

# upscale
y_coords[, 1:2] = y_coords[, 1:2] * 10000
y_items[, 1L] = (y_items[, 1L] * 4400)
y_items[, 2L] = (y_items[, 2L] * 4400)

pdf(file = "images/illustration/EA_initial_solution.pdf", width = 8, height = 3.5)
print(plot_ttp(x_coords, x_items))
dev.off()

pdf(file = "images/illustration/EA_after_mutation.pdf", width = 8, height = 3.5)
print(plot_ttp(y_coords, y_items))
dev.off()
