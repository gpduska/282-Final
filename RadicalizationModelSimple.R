# this is a simplified version of the radicalization model

library(dplyr)

n.population <- 30  #size of population

prob.success.of.take.sd <- 1 # the standard deviation of the normal curve centered at the difference between two peoples power levels

initial.chance.encounter <- 0.01
initial.take.sd <- 0.01
mutation.sd <- 0.01

selection.cutoff <- 2
mutation.cutoff <- 0.8

power.score.index <- (n.population * 2) + 1

# This function instantiates the origional matrix
initial.population.simple <- function(){
  
  power.vector <- c(rep(1, n.population))
  
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
        
        power.take <- abs(rnorm(1, 0, population[i, (n.population + j)]))
        
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
        
        # if the person is trying to take more than the person has, the person being taken from losses all of their power (down to zero)
        if(population[IED.simple[i,1], power.score.index] - IED.simple[i,3] < 0){
          
          population[IED.simple[i,1], power.score.index] <- 0
          population[IED.simple[i,2], power.score.index] <- population[IED.simple[i,2], power.score.index] + population[IED.simple[i,1], power.score.index]
          
        } else {
          
          population[IED.simple[i,1], power.score.index] <- population[IED.simple[i,1], power.score.index] - IED.simple[i,3]
          population[IED.simple[i,2], power.score.index] <- population[IED.simple[i,2], power.score.index] + IED.simple[i,3]
          
        }
        # if the initiator is not successful
      } else {
        
        # if the person tried to take more than they had, the person who failed the attempted take losses all their power
        if(population[IED.simple[i,2], power.score.index] - IED.simple[i,3] < 0){
          
          population[IED.simple[i,1], power.score.index] <- population[IED.simple[i,1], power.score.index] + population[IED.simple[i,2], power.score.index]
          population[IED.simple[i,2], power.score.index] <- 0
          
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

run.n.gens.simple <- function(n){
  
  for(i in 1:n){
    intermediate.exchange.data.simple <- exchange.power.simple(population)
    population <- exchange.response.simple(intermediate.exchange.data.simple)
    population <- genetic.algorithm.simple(population)
  }
  
  return(population)
}

plot.power.by.rank <- function(population){
  
  power.scores <- population[,power.score.index]
  ascending.power.scores <- c()
  
  for(i in 1:n.population){
     
    ascending.power.scores <- append(ascending.power.scores, power.scores[which.min(power.scores)], after = length(ascending.power.scores))
    power.scores <- power.scores[-(which.min(power.scores))]
  }
  
  plot(1:n.population, ascending.power.scores)
}

plot.power.by.person <- function(population){
  
  plot(1:n.population, population[,power.score.index])
}



population <- initial.population.simple()

for(i in 1:50){
  population <- run.n.gens.simple(100)
  plot.power.by.rank(population)
}

