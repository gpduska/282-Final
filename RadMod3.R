# this model uses an exchange algorithm where each agent has a perameter that determines how likely it is
# that they will try to take power from any other agent depending on the two agents difference in power.
# each agent also has a perameter that determines how much the agent will try to take depending on the two agents
# difference in power. 

library(dplyr)
library(ggplot2)

n.population <- 30  #size of population
prob.success.of.take.sd <- 1 # the sd for the curve of how likely it is for a person to succeed in taking power

initial.encounter.slope <- 0
initial.encounter.intercept <- 0

initial.take.attempt.slope <- .5
initial.take.attempt.intercept <- 1

encounter.curve.sd <- .5
take.curve.sd <- .2

selection.cutoff <- 2.4
mutation.cutoff <- 1.7
mutation.sd <- 0.1

# This function instantiates the origional matrix
initial.population.3 <- function(){
  
  power.vector <- c(rep(2, n.population))
  
  initial.encounter.slope.vector <- c(rep(initial.encounter.slope, n.population))
  
  initial.encounter.intercept.vector <- c(rep(initial.encounter.intercept, n.population))
  
  initial.take.attempt.slope.vector <- c(rep(initial.take.attempt.slope, n.population))
  
  initial.take.attempt.intercept.vector <- c(rep(initial.take.attempt.intercept, n.population))
  
  
  return(matrix(c(initial.encounter.slope.vector, initial.encounter.intercept.vector, initial.take.attempt.slope.vector,
                  initial.take.attempt.intercept.vector, power.vector), byrow=F, nrow = n.population))
}

exchange.power.3 <- function(population){
  
  population.vector <- 1:n.population
  
  index.initiated <- c()
  index.initiator <- c()
  exchange.amount <- c()
  
  for(i in 1:n.population){
    
    for(j in population.vector[-i]){
      
      power.diff <- population[i,5] - population[j,5]
      
      if(rnorm(1, population[i,1] * power.diff + population[i,2], encounter.curve.sd) > 0){
        
        power.take <- abs(rnorm(1, population[1,3] * power.diff + population[i,4], take.curve.sd))
        
        index.initiated <- append(index.initiated, j, after = length(index.initiated))
        index.initiator <- append(index.initiator, i, after = length(index.initiator))
        exchange.amount <- append(exchange.amount, power.take, after = length(exchange.amount))
      }
    }
  }
  return(data.frame(index.initiated, index.initiator, exchange.amount))
}


exchange.response.3 <- function(IED.simple){
  
  if(nrow(IED.simple) > 0){
    
    # response to people trying to take their power
    for(i in 1:length(IED.simple[,1])){
      
      power.diff <- population[IED.simple[i,2],5] - population[IED.simple[i,1],5]
      
      # if a random number from a probablility distribution with mean = (power score of person taking power) - (power score of person being taken from)
      # and sd = prob.success.of.take.sd * (the inverse of the power difference) is greater than 0, (if the initiator is successful)
      if(rnorm(1, power.diff, prob.success.of.take.sd * (1/1.1^power.diff)) > 0){
        
        # if the person is trying to take more than the person has, the person being taken from losses all of their power (down to one)
        if(population[IED.simple[i,1], 5] - IED.simple[i,3] < 1){
          
          population[IED.simple[i,1], 5] <- 1
          population[IED.simple[i,2], 5] <- population[IED.simple[i,2], 5] + population[IED.simple[i,1], 5] - 1
          
        } else {
          
          population[IED.simple[i,1], 5] <- population[IED.simple[i,1], 5] - IED.simple[i,3]
          population[IED.simple[i,2], 5] <- population[IED.simple[i,2], 5] + IED.simple[i,3]
          
        }
        # if the initiator is not successful
      } else {
        
        # if the person tried to take more than they had, the person who failed the attempted take losses all their power
        if(population[IED.simple[i,2], 5] - IED.simple[i,3] < 1){
          
          population[IED.simple[i,1], 5] <- population[IED.simple[i,1], 5] + population[IED.simple[i,2], 5] - 1
          population[IED.simple[i,2], 5] <- 1
          
        } else {
          
          population[IED.simple[i,1], 5] <- population[IED.simple[i,1], 5] + IED.simple[i,3]
          population[IED.simple[i,2], 5] <- population[IED.simple[i,2], 5] - IED.simple[i,3]
          
        }
      }
    }
  }
  return(population)
}

genetic.algorithm.3 <- function(population){
  
  
  for(i in 1:n.population){
    # if person i is not above the selection cutoff, the person is regenerated with random perameters between 0 and 1. Their power score stays the same
    if(population[i,5] < selection.cutoff){
      
      population[i,] <- c(rep(runif(1, 0, 1), 4), population[i,5])
      
      # if person i is not above the mutation cutoff, then all of the persons perameters are mutated slightly
    } else if(population[i, 5] < mutation.cutoff){
      
      for(j in 1:4){
        population[i,j] <- population[i,j] + rnorm(1, 0, mutation.sd)
      }
    }
  }
  return(population)
}


run.n.gens.3 <- function(n){
  
  greatest.power.vector <- c()
  median.power.vector <- c()
  lowest.power.vector <- c()
  
  for(i in 1:n){
    intermediate.exchange.data <- exchange.power.3(population)
    population <- exchange.response.3(intermediate.exchange.data)
    print(population)
    population <- genetic.algorithm.3(population)
    print(population)
    greatest.power.vector <- append(greatest.power.vector, max(population[,5]), after = length(greatest.power.vector))
    median.power.vector <- append(median.power.vector, median(population[,5]), after = length(median.power.vector))
    lowest.power.vector <- append(lowest.power.vector, min(population[,5]), after = length(lowest.power.vector))
  }
  return(list(a = population, b = greatest.power.vector, c = median.power.vector, d = lowest.power.vector))
}

plot.power.by.rank.standard <- function(population){
  
  power.scores <- population[,5]
  ascending.power.scores <- c()
  
  for(i in 1:n.population){
    
    ascending.power.scores <- append(ascending.power.scores, power.scores[which.min(power.scores)], after = length(ascending.power.scores))
    power.scores <- power.scores[-(which.min(power.scores))]
    
  }
  
  plot(1:n.population, ascending.power.scores)
}


initial.population.3()
intermediate.exchange.data <- exchange.power.3(population)
population <- exchange.response.3(intermediate.exchange.data)
print(population)
population <- genetic.algorithm.3(population)
print(population)




population <- initial.population.3()
plot.power.by.rank.standard(population)
run.n.gens.3(1)
population
for(i in 1:50){
  run.n.gens.3(100)
  plot.power.by.rank.standard(population)
}
