rm(list = ls())
source("src/init.R")
init()

directory <- "data/20160812/"
robot.list <- get.names()
alg.list <- get.algs()

#############
plot.fitness(directory, alg.list[1:4], robot.list, cx = 3.5)
plot.diversity(directory, alg.list[1:4], robot.list, cx = 3.5)

mean.fit <- best.fitness(directory, alg.list[1:4], robot.list)
mean.fit
percent.fit <- percentages(mean.fit)
percent.fit
num.evals <- max.evals(directory, alg.list[1:4], robot.list)
num.evals
dunn <- dunn.fit(directory, alg.list[1:4], robot.list)
dunn
dunn.test(dunn, kw = TRUE, method = "bonferroni")

##################
##################
##################
robot <- "spider13"
dir1 <- "data/20160812/rlpower/"
dir2 <- "data/20160812/rlpower-btour/"
dir3 <- "data/20160812/rlpower-sigma/"
dir4 <- "data/20160812/rlpower-mix/"
epoch <- generate.epoch(10, 99, 1000, by = 100)

policies.list1 <- yaml.read(dir1, name = robot, sufix = ".policy", 1:10)
policies.list2 <- yaml.read(dir2, name = robot, sufix = ".policy", 1:10)
policies.list3 <- yaml.read(dir3, name = robot, sufix = ".policy", 1:10)
policies.list4 <- yaml.read(dir4, name = robot, sufix = ".policy", 1:10)

gen.div.vector1 <- gen.diversity(policies.list1, epoch, runs = 1:10)
gen.div.vector2 <- gen.diversity(policies.list2, epoch, runs = 1:10)
gen.div.vector3 <- gen.diversity(policies.list3, epoch, runs = 1:10)
gen.div.vector4 <- gen.diversity(policies.list4, epoch, runs = 1:10)

plot(epoch, gen.div.vector2, ylim = c(0,2.3), pch = 0)#, log = "y")
# points(epoch, gen.div.vector2, pch = 0)
points(epoch, gen.div.vector3, pch = 6)
points(epoch, gen.div.vector4, pch = 10)
points(epoch, gen.div.vector1, pch = 1)
legend(500,0.7,c(alg.list[[1]][1], alg.list[[2]][1], alg.list[[3]][1], alg.list[[4]][1]),
       pch = c(1,0,6,10), text.width = 150)
