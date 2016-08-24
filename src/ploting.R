plot.diversity <- function(directory = "data/20160812/", algorithms, robot.list, cx = 3) {
  epoch <- generate.epoch(10, 99, 1000, by = 100)
  for (algorithm in algorithms) {
    dir <- paste(directory, algorithm[1], sep = "/")
    for (robots in robot.list) {
      policies.list1 <- yaml.read(dir, name = robots[1], sufix = ".policy", 1:10)
      gen.div.vector1 <- gen.diversity(policies.list1, epoch, runs = 1:10)
      
      policies.list2 <- yaml.read(dir, name = robots[2], sufix = ".policy", 1:10)
      gen.div.vector2 <- gen.diversity(policies.list2, epoch, runs = 1:10)
      
      policies.list3 <- yaml.read(dir, name = robots[3], sufix = ".policy", 1:10)
      gen.div.vector3 <- gen.diversity(policies.list3, epoch, runs = 1:10)
      
      out.filename <- paste("plots/", algorithm[2], "-", robots[4], "-gdiv.png", sep = "")
      png(filename = out.filename, width = 1290, height = 1109)
      plot(epoch, gen.div.vector1, ylab = "mean of st.dev. over genomes",
           pch = 1, ylim = c(0, 0.7),
           cex.axis = cx, cex.lab = 4)
      points(epoch, gen.div.vector2, pch = 0)
      points(epoch, gen.div.vector3, pch = 6)
      legend(800,0.7,c(robots[1], robots[2], robots[3]),
             pch= c(1, 0, 6), text.width = 150, cex = cx)
      dev.off()
    }
  }
}

plot.fitness <- function(directory = "data/20160812/", algorithms, robot.list, cx = 3) {
  for (algorithm in algorithms) {
    dir <- paste(directory, algorithm[1], sep = "/")
    for(robots in robot.list) {
      logs.list <- yaml.read(dir, name = robots[1], sufix = ".log", 1:10)
      fitnesses.list <- max.fitnesses(logs.list)
      means.vector <- unlist(lapply(fitnesses.list, mean))
      means.cm.vector <- means.vector * 100
      
      logs.list2 <- yaml.read(dir, name = robots[2], sufix = ".log", 1:10)
      fitnesses.list2 <- max.fitnesses(logs.list2)
      means.vector2 <- unlist(lapply(fitnesses.list2, mean))
      means.cm.vector2 <- means.vector2 * 100
      
      logs.list3 <- yaml.read(dir, name = robots[3], sufix = ".log", 1:10)
      fitnesses.list3 <- max.fitnesses(logs.list3)
      means.vector3 <- unlist(lapply(fitnesses.list3, mean))
      means.cm.vector3 <- means.vector3 * 100
      
      out.filename <- paste("plots/", algorithm[2], "-", robots[4], "-fitness.png", sep = "")
      png(filename = out.filename, width = 1290, height = 1109)
      plot(1:length(means.cm.vector), means.cm.vector,
           type = "l", ylim = c(0, 6), xlim = c(0,1000), lty = 1,
           xlab = "num. of evaluations", ylab = "movement speed [cm/s]",
           lwd = 5, cex.axis = cx, cex.lab = 4)
      lines(1:length(means.cm.vector2), means.cm.vector2, lty = 2, lwd = 5)
      lines(1:length(means.cm.vector3), means.cm.vector3, lty = 4, lwd = 5)
      legend(x = 750, y = 6, legend = c(robots[1], robots[2], robots[3]),
             lty= c(1, 2, 4), text.width = 150, lwd = 4, cex = cx)
      dev.off()
    }
  }
}

best.fitness <- function(directory = "data/20160812/", algorithms, robot.list) {
  df <- data.frame()
  for (algorithm in algorithms) {
    dir <- paste(directory, algorithm[1], sep = "/")
    meanlab <- paste("mean", algorithm[2], sep = "-")
    sdlab <- paste("sd", algorithm[2], sep = "-")
    for (robots in robot.list) {
      logs.list1 <- yaml.read(dir, name = robots, sufix = ".log", 1:10)
      evals1 <- max.fitnesses(logs.list1)
      mean1 <- mean(evals1[[1000]]) * 100
      sd1 <- sd(evals1[[1000]]) * 100
      df[as.character(robots[1]), as.character(meanlab)] <- round(mean1, 2)
      df[as.character(robots[1]), as.character(sdlab)] <- round(sd1, 2)
      
      logs.list2 <- yaml.read(dir, name = robots[2], sufix = ".log", 1:10)
      evals2 <- max.fitnesses(logs.list2)
      mean2 <- mean(evals2[[1000]]) * 100
      sd2 <- sd(evals2[[1000]]) * 100
      df[as.character(robots[2]), as.character(meanlab)] <- round(mean2, 2)
      df[as.character(robots[2]), as.character(sdlab)] <- round(sd2, 2)
      
      logs.list3 <- yaml.read(dir, name = robots[3], sufix = ".log", 1:10)
      evals3 <- max.fitnesses(logs.list3)
      mean3 <- mean(evals3[[1000]]) * 100
      sd3 <- sd(evals3[[1000]]) * 100
      df[as.character(robots[3]), as.character(meanlab)] <- round(mean3, 2)
      df[as.character(robots[3]), as.character(sdlab)] <- round(sd3, 2)
      }
  }
  return(df)
}

dunn.fit <- function(directory = "data/20160812/", algorithms, robot.list) {
  df <- data.frame()
  for (algorithm in algorithms) {
    dir <- paste(directory, algorithm[1], sep = "/")
    alg.robots.vec <- vector()
    for(robots in robot.list) {
      logs.list <- yaml.read(dir, name = robots[1], sufix = ".log", 1:10)
      fitnesses.list <- dunn.max(logs.list)
      alg.robots.vec <- c(alg.robots.vec, fitnesses.list)
      
      logs.list2 <- yaml.read(dir, name = robots[2], sufix = ".log", 1:10)
      fitnesses.list2 <- dunn.max(logs.list2)
      alg.robots.vec <- c(alg.robots.vec, fitnesses.list2)
      
      logs.list3 <- yaml.read(dir, name = robots[3], sufix = ".log", 1:10)
      fitnesses.list3 <- dunn.max(logs.list3)
      alg.robots.vec <- c(alg.robots.vec, fitnesses.list3)
    }
    for(i in 1:length(alg.robots.vec)){
      df[i, as.character(algorithm[2])] <- alg.robots.vec[i]
    }
  }
  return(df)
}

max.evals <- function(directory = "data/20160812/", algorithms, robot.list) {
  df <- data.frame()
  for (algorithm in algorithms) {
    dir <- paste(directory, algorithm[1], sep = "/")
    evallab <- paste("mean.max", algorithm[2], sep = "-")
    for (robots in robot.list) {
      logs.list1 <- yaml.read(dir, name = robots[1], sufix = ".log", 1:10)
      evals1 <- eval.to.max(logs.list1)
      df[as.character(robots[1]), as.character(evallab)] <- round(mean(evals1), 2)
      
      logs.list2 <- yaml.read(dir, name = robots[2], sufix = ".log", 1:10)
      evals2 <- eval.to.max(logs.list2)
      df[as.character(robots[2]), as.character(evallab)] <- round(mean(evals2), 2)
      
      logs.list3 <- yaml.read(dir, name = robots[3], sufix = ".log", 1:10)
      evals3 <- eval.to.max(logs.list3)
      df[as.character(robots[3]), as.character(evallab)] <- round(mean(evals3), 2)
    }
  }
  # df <-  rbind(df, colMeans(df))
  #row.names(df[13,]) <- c("means")
  return(df)
}
