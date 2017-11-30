# this is a simplified version of the radicalization model
# each agent uses a separate perameter for how liely it is that they will try to take power from each other agent

library(dplyr)
library(ggplot2)

n.population <- 30  #size of population

prob.success.of.take.sd <- 1 # the standard deviation of the normal curve centered at the difference between two peoples power levels

initial.chance.encounter <- 0.01
initial.take.sd <- 0.05
mutation.sd <- 0.01

selection.cutoff <- 2.4
mutation.cutoff <- 1.7

power.score.index <- (n.population * 2) + 1

# This function instantiates the origional matrix
initial.population.simple <- function(){
  
  power.vector <- c(rep(2, n.population))
  
  chance.encoutner.vector <- rep(initial.chance.encounter, n.population * n.population)
  
  initial.take.sd.vector <- rep(initial.take.sd, n.population * n.population)
  
  
  return(matrix(c(chance.encoutner.vector, initial.take.sd.vector, power.vector), byrow=F, nrow = n.population))
}

exchange.power.simple <- function(population){
  
  population.vector <- 1:n.population
  
  index.initiated <- c()
  index.initiator <- c()
  exchange.amount <- c()
  
  for(i in 1:n.population){
    
    for(j in population.vector[-i]){
      
      if(runif(1, 0, 1) < population[i,j]){
        
        power.take <- abs(rnorm(1, 0, abs(population[i, (n.population + j)])))
        
        index.initiated <- append(index.initiated, j, after = length(index.initiated))
        index.initiator <- append(index.initiator, i, after = length(index.initiator))
        exchange.amount <- append(exchange.amount, power.take, after = length(exchange.amount))
      }
    }
  }
  return(data.frame(index.initiated, index.initiator, exchange.amount))
}

exchange.response.simple <- function(IED.simple){
  
  if(nrow(IED.simple) > 0){
    
    # response to people trying to take their power
    for(i in 1:length(IED.simple[,1])){
      
      # if a random number from a probablility distribution with mean = (power score of person taking power) - (power score of person being taken from)
      # and sd = prob.success.of.take.sd is greater than 0, (if the initiator is successful)
      if(rnorm(1,((population[IED.simple[i,2], power.score.index]) - (population[IED.simple[i,1], power.score.index])), prob.success.of.take.sd) > 0){
        
        # if the person is trying to take more than the person has, the person being taken from losses all of their power (down to one)
        if(population[IED.simple[i,1], power.score.index] - IED.simple[i,3] < 1){
          
          population[IED.simple[i,1], power.score.index] <- 1
          population[IED.simple[i,2], power.score.index] <- population[IED.simple[i,2], power.score.index] + population[IED.simple[i,1], power.score.index] - 1
          
        } else {
          
          population[IED.simple[i,1], power.score.index] <- population[IED.simple[i,1], power.score.index] - IED.simple[i,3]
          population[IED.simple[i,2], power.score.index] <- population[IED.simple[i,2], power.score.index] + IED.simple[i,3]
          
        }
        # if the initiator is not successful
      } else {
        
        # if the person tried to take more than they had, the person who failed the attempted take losses all their power
        if(population[IED.simple[i,2], power.score.index] - IED.simple[i,3] < 1){
          
          population[IED.simple[i,1], power.score.index] <- population[IED.simple[i,1], power.score.index] + population[IED.simple[i,2], power.score.index] - 1
          population[IED.simple[i,2], power.score.index] <- 1
          
        } else {
          
          population[IED.simple[i,1], power.score.index] <- population[IED.simple[i,1], power.score.index] + IED.simple[i,3]
          population[IED.simple[i,2], power.score.index] <- population[IED.simple[i,2], power.score.index] - IED.simple[i,3]
          
        }
      }
    }
  }
  return(population)
}
  
genetic.algorithm.simple <- function(population){
  
  
  for(i in 1:n.population){
    # if person i is not above the selection cutoff, the person is regenerated with random perameters between 0 and 1. Their power score stays the same
    if(population[i, power.score.index] < selection.cutoff){
      population[i,] <- c(rep(runif(1, 0, .1), n.population * 2), population[i, power.score.index])
      
      # if person i is not above the mutation cutoff, then all of the persons perameters are mutated slightly
    } else if(population[i, power.score.index] < mutation.cutoff){
      
      for(j in 1:(n.population * 2)){
        population[i,j] <- population[i,j] + rnorm(1, 0, mutation.sd)
      }
    }
  }
  return(population)
}

genetic.algorithm.simple.copy.up <- function(population){
  
  
  power.scores <- population[,power.score.index]
  index.vector <- 1:n.population
  index.by.ascending.power <- c()
  
  for(i in 1:n.population){
    
    index.by.ascending.power <- append(index.by.ascending.power, index.vector[which.min(power.scores)], after = length(index.by.ascending.power))
    index.vector <- index.vector[-(which.min(power.scores))]
    power.scores <- power.scores[-(which.min(power.scores))]
    
  }
  
  for(i in index.by.ascending.power[-n.population]){
    
    index.to.copy <- index.by.ascending.power[which(index.by.ascending.power == i) + 1]
    #index.most.powerful <- index.by.ascending.power[n.population]
    
    population[i,] <- c(population[index.to.copy, 1] + rnorm(1, 0, mutation.sd), population[index.to.copy, 2] + rnorm(1, 0, mutation.sd), 
                        population[index.to.copy, 3] + rnorm(1, 0, mutation.sd), population[index.to.copy, 4] + rnorm(1, 0, mutation.sd), 
                        population[index.to.copy, 5] + rnorm(1, 0, mutation.sd), population[index.to.copy, 6] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 7] + rnorm(1, 0, mutation.sd), population[index.to.copy, 8] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 9] + rnorm(1, 0, mutation.sd), population[index.to.copy, 10] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 11] + rnorm(1, 0, mutation.sd), population[index.to.copy, 12] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 13] + rnorm(1, 0, mutation.sd), population[index.to.copy, 14] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 15] + rnorm(1, 0, mutation.sd), population[index.to.copy, 16] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 17] + rnorm(1, 0, mutation.sd), population[index.to.copy, 18] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 19] + rnorm(1, 0, mutation.sd), population[index.to.copy, 20] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 21] + rnorm(1, 0, mutation.sd), population[index.to.copy, 22] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 23] + rnorm(1, 0, mutation.sd), population[index.to.copy, 24] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 25] + rnorm(1, 0, mutation.sd), population[index.to.copy, 26] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 27] + rnorm(1, 0, mutation.sd), population[index.to.copy, 28] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 29] + rnorm(1, 0, mutation.sd), population[index.to.copy, 30] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 31] + rnorm(1, 0, mutation.sd), population[index.to.copy, 32] + rnorm(1, 0, mutation.sd), 
                        population[index.to.copy, 33] + rnorm(1, 0, mutation.sd), population[index.to.copy, 34] + rnorm(1, 0, mutation.sd), 
                        population[index.to.copy, 35] + rnorm(1, 0, mutation.sd), population[index.to.copy, 36] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 37] + rnorm(1, 0, mutation.sd), population[index.to.copy, 38] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 39] + rnorm(1, 0, mutation.sd), population[index.to.copy, 40] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 41] + rnorm(1, 0, mutation.sd), population[index.to.copy, 42] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 43] + rnorm(1, 0, mutation.sd), population[index.to.copy, 44] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 45] + rnorm(1, 0, mutation.sd), population[index.to.copy, 46] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 47] + rnorm(1, 0, mutation.sd), population[index.to.copy, 48] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 49] + rnorm(1, 0, mutation.sd), population[index.to.copy, 50] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 51] + rnorm(1, 0, mutation.sd), population[index.to.copy, 52] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 53] + rnorm(1, 0, mutation.sd), population[index.to.copy, 54] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 55] + rnorm(1, 0, mutation.sd), population[index.to.copy, 56] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 57] + rnorm(1, 0, mutation.sd), population[index.to.copy, 58] + rnorm(1, 0, mutation.sd),
                        population[index.to.copy, 59] + rnorm(1, 0, mutation.sd), population[index.to.copy, 60] + rnorm(1, 0, mutation.sd),
                        population[i, power.score.index])
    
    #if(population[i,power.score.index] == 0){
    #  population[i, index.most.powerful] <- 0.5
    #  population[i, (n.population + index.most.powerful)] <- 0.5
    #}
  }
  return(population)
}

run.n.gens.simple.v1 <- function(n){
  
  greatest.power.vector <- c()
  median.power.vector <- c()
  lowest.power.vector <- c()
  
  for(i in 1:n){
    intermediate.exchange.data.simple <- exchange.power.simple(population)
    population <- exchange.response.simple(intermediate.exchange.data.simple)
    population <- genetic.algorithm.simple(population)
    greatest.power.vector <- append(greatest.power.vector, max(population[,61]), after = length(greatest.power.vector))
    median.power.vector <- append(median.power.vector, median(population[,61]), after = length(median.power.vector))
    lowest.power.vector <- append(lowest.power.vector, min(population[,61]), after = length(lowest.power.vector))
  }
  
  return(list(a = population, b = greatest.power.vector, c = median.power.vector, d = lowest.power.vector))
}

run.n.gens.simple.v2 <- function(n){
  
  for(i in 1:n){
    intermediate.exchange.data.simple <- exchange.power.simple(population)
    population <- exchange.response.simple(intermediate.exchange.data.simple)
    population <- genetic.algorithm.simple.copy.up(population)
  }
  
  return(population)
}

plot.power.by.rank.simple.plot <- function(population){
  
  power.scores <- population[,power.score.index]
  ascending.power.scores <- c()
  
  for(i in 1:n.population){
    
    ascending.power.scores <- append(ascending.power.scores, power.scores[which.min(power.scores)], after = length(ascending.power.scores))
    power.scores <- power.scores[-(which.min(power.scores))]
    
  }
  
  plot(1:n.population, ascending.power.scores)
}

plot.power.by.rank.ggplot2 <- function(population){
  
  power.scores <- population[,power.score.index]
  index.vector <- 1:n.population
  ascending.power.scores <- c()
  index.by.ascending.power <- c()
  
  for(i in 1:n.population){
    
    ascending.power.scores <- append(ascending.power.scores, power.scores[which.min(power.scores)], after = length(ascending.power.scores))
    index.by.ascending.power <- append(index.by.ascending.power, index.vector[which.min(power.scores)], after = length(index.by.ascending.power))
    index.vector <- index.vector[-(which.min(power.scores))]
    power.scores <- power.scores[-(which.min(power.scores))]
    
  }
  
  d <- data.frame(x = 1:n.population, y = ascending.power.scores, names = index.by.ascending.power)
  ggplot(d, aes(x,y)) + geom_text(aes(label=names))
}

plot.power.by.person <- function(population){
  
  plot(1:n.population, population[,power.score.index])
}

population <- initial.population.simple()
plot.power.by.rank.simple.plot(population)

for(i in 1:3){
  results <- run.n.gens.simple.v1(100)
  population <- results$a
  plot.power.by.rank.simple.plot(population)
}

results <- run.n.gens.simple.v1(100)
population <- results$a
greatest.power.vector <- results$b
median.power.vector <- results$c
lowest.power.vector <- results$d

plot.power.by.rank.simple.plot(population)
plot(1:50000, greatest.power.vector)
plot(1:50000, median.power.vector)
plot(1:50000, lowest.power.vector)
population <- run.n.gens.simple.v2(1)
plot.power.by.rank.ggplot2(population)
population
max(results$a[,61])

print(population)
