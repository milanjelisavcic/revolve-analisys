# main.R utility functions
init <- function() {
  library(dunn.test)
  source("src/conversion.R")
  source("src/genomes.R")
  source("src/ploting.R")
  source("src/fitness.R")
  source("src/percentages.R")
}

get.names <- function() {
  spiders <- c("spider9", "spider13", "spider17", "spiders")
  geckos <- c("gecko7", "gecko12", "gecko17", "geckos")
  snakes <- c("snake5", "snake7", "snake9", "snakes")
  babys <- c("babyA", "babyB", "babyC", "babys")
  
  return(list(spiders, geckos, snakes, babys))
}

get.algs <- function() {
  alg.a <- c("rlpower", "algA", "Algorithm A")
  alg.b <- c("rlpower-btour", "algB", "Algorithm B")
  alg.c <- c("rlpower-sigma", "algC", "Algorithm C")
  alg.d <- c("rlpower-mix", "algD", "Algorithm D")
  
  return(list(alg.a, alg.b, alg.c, alg.d))
}