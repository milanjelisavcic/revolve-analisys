#' Calculates vector of standard deviatons for every gene in population
#' 
#' @param 'population' a data.frame that contains genomes of the population
#' 
#' @return a vector of standard deviations for an every gene in the population
population.sd <- function(population) {
  genome.sd <- vector()
  for (i in 1:length(population[[1]]$policy)) {
    gene.vector <- vector()
    for (j in 1:length(population)) {
      gene.vector <- c(gene.vector, population[[j]]$policy[[i]])
    }
    genome.sd <- c(genome.sd, sd(gene.vector))
  }
  return(genome.sd)
}

#' Generates sequence of indiccies that represent begin and end of each epoch
#' 
#' @param 
#' @param 
#' @param 
#' @param 
#' 
#' @return a vector of numbers that represent begin and end of each epoch
generate.epoch <- function(begin1 = 10, begin2 = 99, to = 1000, by = 100) {
  e.begin <- seq(from = begin1, to = to, by = by)
  e.end <- seq(from = begin2, to = to, by = by)
  epoch <- data.frame(c = e.begin, d = e.end)
  return(c(t(epoch)))
}

gen.diversity <- function(files.list, epoch, runs = 1:10) {
  expr.means.vector <- vector()
  for (e in epoch) {
    gene.means.vector <- vector()  
    for (r in runs) {
      experiment <- files.list[[r]]
      population <- experiment[[e]]$population
      gene.sd.vector <- population.sd(population)
      gene.means.vector <- c(gene.means.vector, mean(gene.sd.vector))
    }
    expr.means.vector <- c(expr.means.vector, mean(gene.means.vector))
  }
  return(expr.means.vector)
}