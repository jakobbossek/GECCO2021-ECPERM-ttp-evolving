# Check how often desired ranking was achieved
check_ranking_order = function(ranking, perfs) {
  n = length(ranking)
  res = logical(n)
  for (i in seq_len(n)) {
    algoids = as.integer(strsplit(ranking[i], split = "-")[[1L]])
    satisfied = TRUE
    for (j in (1:(length(algoids) - 1L))) {
      BBmisc::catf("%.2f vs. %.2f", perfs[i, algoids[j]], perfs[i, algoids[j+1]])
      satisfied = satisfied & (perfs[i, algoids[j]] >= perfs[i, algoids[j+1]])
    }
    res[i] = satisfied
  }
  return(res)
}

determine_ranking_noorder = function(perfs) {
  n = nrow(perfs)
  ranking = character(n)
  for (i in seq_len(n)) {
    ord = order(perfs[i, ], decreasing = TRUE)
    #algord = ALL_ALGORITHMS[ord]
    algord = c("S2", "S4", "C2")[ord]
    ranking[i] = BBmisc::collapse(algord, sep = " > ")
    #ranking[i] = BBmisc::collapse(paste0("A", algord), sep = " > ")
  }
  return(ranking)
}

decode_ranking = function(xs, types) {
  idlist = strsplit(xs,  split = "-")
  sapply(1:length(idlist), function(i) {
    ids = as.integer(idlist[[i]])
    type = types[i]
    #algos = paste0("A", ALL_ALGORITHMS[ids])
    algos = c("S2", "S4", "C2")[ids]
    if (type == "pairwise") {
      return(BBmisc::collapse(algos, sep = " > "))
    } else if (type == "gap-to-second-best") {
      return(paste0(algos[1L], " > 2nd best"))
    } else if (type == "no-order") {
      return("no-order")
    } else if (type == "explicit-ranking") {
      return(BBmisc::collapse(algos, sep = " > "))
    } else {
      BBmisc::stopf("[decode_ranking] This case (%s) should not occur!", type)
    }
  })
}

plot_ttp = function(x) {
  plot_ttp_internal = function(x, aes_x, aes_y) {
    g = ggplot(data = x, aes_string(x = aes_x, y = aes_y))
    g = g + geom_point(shape = 1)
    g = g + theme_bw()
    return(g)
  }
  gcoords = plot_ttp_internal(x$coordinates, "V2", "V3") + xlab("node x-coordinate") + ylab("node y-coordinate") + xlim(c(0, 10000)) + ylim(c(0, 10000))
  gitems  = plot_ttp_internal(x$items, "weight", "profit") + xlab("weight") + ylab("profit") + xlim(c(0, 4040)) + ylim(c(0, 4400))
  gridExtra::grid.arrange(gcoords, gitems, nrow = 1L)
}
