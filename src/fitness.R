max.fitnesses <- function(fitnesses.list, id = 1:10) {
  maxf.runs.list <- list()
  for (i in 1:length(fitnesses.list[[1]])) {
    maxf.runs.list[[i]] <- vector()
    maxf.vector <- vector()
    for (j in 1:length(fitnesses.list[id])) {
      maxf.vector <- c(maxf.vector, max(fitnesses.list[[j]][[i]]$velocities))
    }
    maxf.runs.list[[i]] <- maxf.vector
  }
  return(maxf.runs.list)
}

dunn.max <- function(fitnesses.list, id = 1:10) {
  maxf.vector <- vector()
  for (j in 1:length(fitnesses.list[id])) {
    maxf.vector <- c(maxf.vector, max(fitnesses.list[[j]][[1000]]$velocities))
  }
  return(maxf.vector)
}

eval.to.max <- function(logs.list, id = 1:10) {
  evals.vector <- vector()
  for(experiment in logs.list[id]) {
    maxfit <- max(experiment[[1000]]$velocities)
    i <- 1
    while (max(experiment[[i]]$velocities) < maxfit) {
      i <- i + 1
    }
    evals.vector <- c(evals.vector, i)
  }
  return(evals.vector)
}