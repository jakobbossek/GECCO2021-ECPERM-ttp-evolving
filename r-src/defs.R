ALL_ALGORITHMS = c(21, 23, 26)
N_RUNS_DURING_EVOLUTION = 5L
N_RUNS_FOR_EVALUATION = 10L # to discuss with MW
MAX_TIME_FOR_EACH_SOLVER_RUN = 10
MAX_ITERS_WITHOUT_IMPROVEMENT = 100000L # to discuss with MW

INSTANCE_SIZES = 100L
IPN = c(1, 3, 5, 10)
FITNESS_TYPES = c("gap-to-second-best", "no-order", "explicit-ranking")
N_INSTANCES = 50L

WALLTIME = 180 #60 * 60 * 47.5

R_BOUNDS = c(0, 1000)
C_BOUNDS = c(1, 10) # left open
V_MIN = 0.1
V_MAX = 1

WEIGHT_BOUNDS = c(1, 4040)
PROFIT_BOUNDS = c(1, 4400)
NODE_COORDINATE_BOUNDS = c(0, 10000)

OUTPUT_PATH = "/Users/bossek/scratch/ttp-evolving/"
