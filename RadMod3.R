# this model uses an exchange algorithm where each agent has a perameter that determines how likely it is
# that they will try to take power from any other agent depending on the two agents difference in power.
# each agent also has a perameter that determines how much the agent will try to take depending on the two agents
# difference in power. 


# 11/22/2017
# new algorithm is:

# population starts off with random power score
# population goes through each other member and decides whether they want to attempt to take power from each other person
# go through the populaion and decide whether each attempted power grab was successful or not
# after this process, each player will have a diferent amount of power than they started off with
# during the genetic algorithm, a bottom percentage of players get a point
# after a certain number of rounds, all players who have over a certain number of points get re-perameterized
# (the point system is implemented so that a player who is using a good strategy but gets a very low initial power score doesn't
# get imediatly eliminated.)

library(dplyr)
library(ggplot2)

n.population <- 30  #size of population
prob.success.of.take.sd <- 0.2 # the sd for the curve of how likely it is for a person to succeed in taking power

initial.encounter.slope <- 1
initial.encounter.intercept <- 0

initial.take.attempt.slope <- .5
initial.take.attempt.intercept <- 0

encounter.curve.sd <- .5
take.curve.sd <- .2

mutation.sd <- 0.01

loss.cutoff <- 8
rounds.per.gen <- 10

# This function instantiates the origional matrix
initial.population.3 <- function(){
  
  power.vector.beginning.of.round <- c(abs(rnorm(n.population, 0, 1)))
  
  power.vector.end.of.round <- c(rep(0, n.population))
  
  initial.encounter.slope.vector <- c(rnorm(n.population, initial.encounter.slope, 0.5))
  
  initial.encounter.intercept.vector <- c(rnorm(n.population, initial.encounter.intercept, 0.5))
  
  initial.take.attempt.slope.vector <- c(rnorm(n.population, initial.take.attempt.slope, 0.5))
  
  initial.take.attempt.intercept.vector <- c(rnorm(n.population, initial.take.attempt.intercept, 0.5))
  
  initial.loss.counter.vector <- c(rep(0, n.population))
  
  return(matrix(c(initial.encounter.slope.vector, initial.encounter.intercept.vector, initial.take.attempt.slope.vector,
                  initial.take.attempt.intercept.vector, power.vector.beginning.of.round, power.vector.end.of.round,
                  initial.loss.counter.vector), byrow=F, nrow = n.population))
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


exchange.response.3 <- function(IED, population){
  
  if(nrow(IED) > 0){
    
    # response to people trying to take their power
    for(i in 1:length(IED[,1])){
      
      power.diff <- population[IED[i,2],5] - population[IED[i,1],5]
      
      # if a random number from a probablility distribution with mean = (power score of person taking power) - (power score of person being taken from)
      # and sd = prob.success.of.take.sd * (the inverse of the power difference) is greater than 0, (if the initiator is successful)
      if(rnorm(1, power.diff, prob.success.of.take.sd) > 0){
        
        # if the person is trying to take more than the person has, the person being taken from losses all of their power (down to zero)
        if(population[IED[i,1], 5] - IED[i,3] < 0){
          
          population[IED[i,1], 6] <- 0
          population[IED[i,2], 6] <- population[IED[i,2], 5] + population[IED[i,1], 5]
          
        } else {
          
          population[IED[i,1], 6] <- population[IED[i,1], 5] - IED[i,3]
          population[IED[i,2], 6] <- population[IED[i,2], 5] + IED[i,3]
          
        }
        # if the initiator is not successful
      } else {
        
        # if the person tried to take more than they had, the person who failed the attempted take losses all their power
        if(population[IED[i,2], 5] - IED[i,3] < 0){
          
          population[IED[i,1], 6] <- population[IED[i,1], 5] + population[IED[i,2], 5]
          population[IED[i,2], 6] <- 0
          
        } else {
          
          population[IED[i,1], 6] <- population[IED[i,1], 5] + IED[i,3]
          population[IED[i,2], 6] <- population[IED[i,2], 5] - IED[i,3]
          
        }
      }
    }
  }
  return(population)
}


loss.counter <- function(population){
  
  for(i in 1:n.population){
    
    if((population[i,5] - population[i,6]) > 0){
      
      population[i,7] <- population[i,7] + 1
    }
  }
  return(population)
}


power.reset <- function(population){
  
  # resets everyones power scores for the next round
  for(i in 1:n.population){
    
    population[i,5] <- abs(rnorm(1, 0, 1))
    population[i,6] <- 0
  }
  return(population)
}

genetic.algorithm.3 <- function(population){
  
  for(i in 1:n.population){
    
    random.person <- NA
    
    # if person i is not above the loss cutoff, the person is regenerated with mutations on random people's peramteters from the surviving
    # population. Their power score is also randomly generated for the next round, and their loss counter is set to zero
    if(population[i,7] >= loss.cutoff){
      
     while(is.na(random.person)){
      
       random <- sample(1:n.population, 1, replace = T)
      
      if(population[random,7] < loss.cutoff){
        
        random.person <- random
      }
    }
      
      population[i,] <- c(population[random.person,1] + rnorm(1, 0, mutation.sd), population[random.person,2] + rnorm(1, 0, mutation.sd),
                          population[random.person,3] + rnorm(1, 0, mutation.sd), population[random.person,4] + rnorm(1, 0, mutation.sd),
                          abs(rnorm(1, 0, 1)), 0, 0)
      
    } else {
      population[i,5] <- abs(rnorm(1, 0, 1))
      population[i,6] <- 0
      population[i,7] <- 0
    }
  }
  return(population)
}


run.n.gens.3 <- function(n, population){
  
  average.encounter.slope <- c()
  average.encounter.intercept <- c()
  average.take.attempt.slope <- c()
  average.take.attempt.intercept <- c()
  
  for(i in 1:n){
    
    for(j in 1:rounds.per.gen){
      intermediate.exchange.data <- exchange.power.3(population)
      population <- exchange.response.3(intermediate.exchange.data, population)
      population <- loss.counter(population)
      population <- power.reset(population)
    }
    
    average.encounter.slope <- append(average.encounter.slope, mean(population[,1]), after = length(average.encounter.slope))
    average.encounter.intercept <- append(average.encounter.intercept, mean(population[,2]), after = length(average.encounter.intercept))
    average.take.attempt.slope <- append(average.take.attempt.slope, mean(population[,3]), after = length(average.take.attempt.slope))
    average.take.attempt.intercept <- append(average.take.attempt.intercept, mean(population[,4]), after = length(average.take.attempt.intercept))
    
    population <- genetic.algorithm.3(population)
  }
  return(list(a = population, b = average.encounter.slope, c = average.encounter.intercept, d = average.take.attempt.slope, 
              e = average.take.attempt.intercept))
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

population <- initial.population.3()
results <- run.n.gens.3(5000, population)


population <- results$a
average.encounter.slope <- results$b
average.encounter.intercept <- results$c
average.take.attempt.slope <- results$d
average.take.attempt.intercept <- results$e

plot(1:5000, average.encounter.slope)
plot(1:5000, average.encounter.intercept)
plot(1:5000, average.take.attempt.slope)
plot(1:5000, average.take.attempt.intercept)

mean(average.encounter.slope[-(1:500)])
mean(average.encounter.intercept[-(1:500)])
mean(average.take.attempt.slope[-(1:500)])
mean(average.take.attempt.intercept[-(1:500)])
