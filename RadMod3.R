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
rounds.per.gen <- 10

# This function instantiates the origional matrix
initial.population.3 <- function(){
  
  power.vector.beginning.of.round <- c(abs(rnorm(n.population, 0, 1)))
  
  power.vector.end.of.round <- c(rep(0, n.population))
  
  initial.encounter.slope.vector <- c(rnorm(n.population, initial.encounter.slope, 1))
  
  initial.encounter.intercept.vector <- c(rnorm(n.population, initial.encounter.intercept, 1))
  
  initial.take.attempt.slope.vector <- c(rnorm(n.population, initial.take.attempt.slope, 1))
  
  initial.take.attempt.intercept.vector <- c(rnorm(n.population, initial.take.attempt.intercept, 1))
  
  initial.loss.counter.vector <- c(rep(0, n.population))
  
  return(matrix(c(initial.encounter.slope.vector, initial.encounter.intercept.vector, initial.take.attempt.slope.vector,
                  initial.take.attempt.intercept.vector, power.vector.beginning.of.round, power.vector.end.of.round,
                  initial.loss.counter.vector), byrow=F, nrow = n.population))
}


exchange.power.3 <- function(population){
  
  population.vector <- sample(1:n.population, size = n.population, replace = FALSE)
  
  index.initiated <- c()
  index.initiator <- c()
  exchange.amount <- c()
  
  for(i in population.vector){
    
    for(j in population.vector[-i]){
      
      power.diff <- population[i,5] - population[j,5]
      
      if(rnorm(1, population[i,1] * power.diff + population[i,2], encounter.curve.sd) > 0){
        
        power.take <- abs(rnorm(1, population[1,3] * power.diff + population[i,4], take.curve.sd))
        
        # if a random number from a probablility distribution with mean = (power score of person taking power) - (power score of person being taken from)
        # and sd = prob.success.of.take.sd * (the inverse of the power difference) is greater than 0, (if the initiator is successful)
        if(rnorm(1, power.diff, prob.success.of.take.sd) > 0){
          
          # if the person is trying to take more than the person has, the person being taken from losses all of their power (down to zero)
          if(population[j,5] - power.take < 0){
            
            population[j, 6] <- 0
            population[i, 6] <- population[i, 5] + population[i, 5]
            
          } else {
            
            population[j, 6] <- population[j, 5] - power.take
            population[i, 6] <- population[i, 5] + power.take
            
          }
          # if the initiator is not successful
        } else {
          
          # if the person tried to take more than they had, the person who failed the attempted take losses all their power
          if(population[i, 5] - power.take < 0){
            
            population[j, 6] <- population[j, 5] + power.take
            population[i, 6] <- 0
            
          } else {
            
            population[j, 6] <- population[j, 5] + power.take
            population[i, 6] <- population[i, 5] - power.take
            
          }
        }
        
        #index.initiated <- append(index.initiated, j, after = length(index.initiated))
        #index.initiator <- append(index.initiator, i, after = length(index.initiator))
        #exchange.amount <- append(exchange.amount, power.take, after = length(exchange.amount))
      }
    }
  }
  return(population)
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
  
  loss.cutoff <- median(population[,7]) + 2
  
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
      population[i,] <- c(population[i,1] + rnorm(1, 0, mutation.sd), population[i,2] + rnorm(1, 0, mutation.sd),
                          population[i,3] + rnorm(1, 0, mutation.sd), population[i,4] + rnorm(1, 0, mutation.sd),
                          abs(rnorm(1, 0, 1)), 0, 0)
    }
  }
  return(population)
}


run.n.gens.3 <- function(n, population){
  
  average.encounter.slope <- c()
  average.encounter.intercept <- c()
  average.take.attempt.slope <- c()
  average.take.attempt.intercept <- c()
  median.num.losses <- c()
  
  agent.1.encounter.slope <- c()
  agent.1.encounter.intercept <- c()
  agent.1.take.attempt.slope <- c()
  agent.1.take.attempt.intercept <- c()
  
  
  agent.1.encounter.slope <- c()
  agent.2.encounter.slope <- c()
  agent.3.encounter.slope <- c()
  agent.4.encounter.slope <- c()
  agent.5.encounter.slope <- c()
  agent.6.encounter.slope <- c()
  agent.7.encounter.slope <- c()
  agent.8.encounter.slope <- c()
  agent.9.encounter.slope <- c()
  agent.10.encounter.slope <- c()
  agent.11.encounter.slope <- c()
  agent.12.encounter.slope <- c()
  agent.13.encounter.slope <- c()
  agent.14.encounter.slope <- c()
  agent.15.encounter.slope <- c()
  agent.16.encounter.slope <- c()
  agent.17.encounter.slope <- c()
  agent.18.encounter.slope <- c()
  agent.19.encounter.slope <- c()
  agent.20.encounter.slope <- c()
  agent.21.encounter.slope <- c()
  agent.22.encounter.slope <- c()
  agent.23.encounter.slope <- c()
  agent.24.encounter.slope <- c()
  agent.25.encounter.slope <- c()
  agent.26.encounter.slope <- c()
  agent.27.encounter.slope <- c()
  agent.28.encounter.slope <- c()
  agent.29.encounter.slope <- c()
  agent.30.encounter.slope <- c()
  
  agent.1.encounter.intercept <- c()
  agent.2.encounter.intercept <- c()
  agent.3.encounter.intercept <- c()
  agent.4.encounter.intercept <- c()
  agent.5.encounter.intercept <- c()
  agent.6.encounter.intercept <- c()
  agent.7.encounter.intercept <- c()
  agent.8.encounter.intercept <- c()
  agent.9.encounter.intercept <- c()
  agent.10.encounter.intercept <- c()
  agent.11.encounter.intercept <- c()
  agent.12.encounter.intercept <- c()
  agent.13.encounter.intercept <- c()
  agent.14.encounter.intercept <- c()
  agent.15.encounter.intercept <- c()
  agent.16.encounter.intercept <- c()
  agent.17.encounter.intercept <- c()
  agent.18.encounter.intercept <- c()
  agent.19.encounter.intercept <- c()
  agent.20.encounter.intercept <- c()
  agent.21.encounter.intercept <- c()
  agent.22.encounter.intercept <- c()
  agent.23.encounter.intercept <- c()
  agent.24.encounter.intercept <- c()
  agent.25.encounter.intercept <- c()
  agent.26.encounter.intercept <- c()
  agent.27.encounter.intercept <- c()
  agent.28.encounter.intercept <- c()
  agent.29.encounter.intercept <- c()
  agent.30.encounter.intercept <- c()
  
  agent.1.take.attempt.slope <- c()
  agent.2.take.attempt.slope <- c()
  agent.3.take.attempt.slope <- c()
  agent.4.take.attempt.slope <- c()
  agent.5.take.attempt.slope <- c()
  agent.6.take.attempt.slope <- c()
  agent.7.take.attempt.slope <- c()
  agent.8.take.attempt.slope <- c()
  agent.9.take.attempt.slope <- c()
  agent.10.take.attempt.slope <- c()
  agent.11.take.attempt.slope <- c()
  agent.12.take.attempt.slope <- c()
  agent.13.take.attempt.slope <- c()
  agent.14.take.attempt.slope <- c()
  agent.15.take.attempt.slope <- c()
  agent.16.take.attempt.slope <- c()
  agent.17.take.attempt.slope <- c()
  agent.18.take.attempt.slope <- c()
  agent.19.take.attempt.slope <- c()
  agent.20.take.attempt.slope <- c()
  agent.21.take.attempt.slope <- c()
  agent.22.take.attempt.slope <- c()
  agent.23.take.attempt.slope <- c()
  agent.24.take.attempt.slope <- c()
  agent.25.take.attempt.slope <- c()
  agent.26.take.attempt.slope <- c()
  agent.27.take.attempt.slope <- c()
  agent.28.take.attempt.slope <- c()
  agent.29.take.attempt.slope <- c()
  agent.30.take.attempt.slope <- c()
  
  agent.1.take.attempt.intercept <- c()
  agent.2.take.attempt.intercept <- c()
  agent.3.take.attempt.intercept <- c()
  agent.4.take.attempt.intercept <- c()
  agent.5.take.attempt.intercept <- c()
  agent.6.take.attempt.intercept <- c()
  agent.7.take.attempt.intercept <- c()
  agent.8.take.attempt.intercept <- c()
  agent.9.take.attempt.intercept <- c()
  agent.10.take.attempt.intercept <- c()
  agent.11.take.attempt.intercept <- c()
  agent.12.take.attempt.intercept <- c()
  agent.13.take.attempt.intercept <- c()
  agent.14.take.attempt.intercept <- c()
  agent.15.take.attempt.intercept <- c()
  agent.16.take.attempt.intercept <- c()
  agent.17.take.attempt.intercept <- c()
  agent.18.take.attempt.intercept <- c()
  agent.19.take.attempt.intercept <- c()
  agent.20.take.attempt.intercept <- c()
  agent.21.take.attempt.intercept <- c()
  agent.22.take.attempt.intercept <- c()
  agent.23.take.attempt.intercept <- c()
  agent.24.take.attempt.intercept <- c()
  agent.25.take.attempt.intercept <- c()
  agent.26.take.attempt.intercept <- c()
  agent.27.take.attempt.intercept <- c()
  agent.28.take.attempt.intercept <- c()
  agent.29.take.attempt.intercept <- c()
  agent.30.take.attempt.intercept <- c()
  
  agent.1.num.losses <- c()
  agent.2.num.losses <- c()
  agent.3.num.losses <- c()
  agent.4.num.losses <- c()
  agent.5.num.losses <- c()
  agent.6.num.losses <- c()
  agent.7.num.losses <- c()
  agent.8.num.losses <- c()
  agent.9.num.losses <- c()
  agent.10.num.losses <- c()
  agent.11.num.losses <- c()
  agent.12.num.losses <- c()
  agent.13.num.losses <- c()
  agent.14.num.losses <- c()
  agent.15.num.losses <- c()
  agent.16.num.losses <- c()
  agent.17.num.losses <- c()
  agent.18.num.losses <- c()
  agent.19.num.losses <- c()
  agent.20.num.losses <- c()
  agent.21.num.losses <- c()
  agent.22.num.losses <- c()
  agent.23.num.losses <- c()
  agent.24.num.losses <- c()
  agent.25.num.losses <- c()
  agent.26.num.losses <- c()
  agent.27.num.losses <- c()
  agent.28.num.losses <- c()
  agent.29.num.losses <- c()
  agent.30.num.losses <- c()
  
  for(i in 1:n){
    
    for(j in 1:rounds.per.gen){
      population <- exchange.power.3(population)
      population <- loss.counter(population)
      population <- power.reset(population)
    }
    
    
    average.encounter.slope <- append(average.encounter.slope, mean(population[,1]), after = length(average.encounter.slope))
    average.encounter.intercept <- append(average.encounter.intercept, mean(population[,2]), after = length(average.encounter.intercept))
    average.take.attempt.slope <- append(average.take.attempt.slope, mean(population[,3]), after = length(average.take.attempt.slope))
    average.take.attempt.intercept <- append(average.take.attempt.intercept, mean(population[,4]), after = length(average.take.attempt.intercept))
    median.num.losses <- append(median.num.losses, median(population[,7]), after = length(median.num.losses))
    
    
    agent.1.encounter.slope <- append(agent.1.encounter.slope, population[1,1], after = length(agent.1.encounter.slope))
    agent.2.encounter.slope <- append(agent.2.encounter.slope, population[2,1], after = length(agent.2.encounter.slope))
    agent.3.encounter.slope <- append(agent.3.encounter.slope, population[3,1], after = length(agent.3.encounter.slope))
    agent.4.encounter.slope <- append(agent.4.encounter.slope, population[4,1], after = length(agent.4.encounter.slope))
    agent.5.encounter.slope <- append(agent.5.encounter.slope, population[5,1], after = length(agent.5.encounter.slope))
    agent.6.encounter.slope <- append(agent.6.encounter.slope, population[6,1], after = length(agent.6.encounter.slope))
    agent.7.encounter.slope <- append(agent.7.encounter.slope, population[7,1], after = length(agent.7.encounter.slope))
    agent.8.encounter.slope <- append(agent.8.encounter.slope, population[8,1], after = length(agent.8.encounter.slope))
    agent.9.encounter.slope <- append(agent.9.encounter.slope, population[9,1], after = length(agent.9.encounter.slope))
    agent.10.encounter.slope <- append(agent.10.encounter.slope, population[10,1], after = length(agent.10.encounter.slope))
    agent.11.encounter.slope <- append(agent.11.encounter.slope, population[11,1], after = length(agent.11.encounter.slope))
    agent.12.encounter.slope <- append(agent.12.encounter.slope, population[12,1], after = length(agent.12.encounter.slope))
    agent.13.encounter.slope <- append(agent.13.encounter.slope, population[13,1], after = length(agent.13.encounter.slope))
    agent.14.encounter.slope <- append(agent.14.encounter.slope, population[14,1], after = length(agent.14.encounter.slope))
    agent.15.encounter.slope <- append(agent.15.encounter.slope, population[15,1], after = length(agent.15.encounter.slope))
    agent.16.encounter.slope <- append(agent.16.encounter.slope, population[16,1], after = length(agent.16.encounter.slope))
    agent.17.encounter.slope <- append(agent.17.encounter.slope, population[17,1], after = length(agent.17.encounter.slope))
    agent.18.encounter.slope <- append(agent.18.encounter.slope, population[18,1], after = length(agent.18.encounter.slope))
    agent.19.encounter.slope <- append(agent.19.encounter.slope, population[19,1], after = length(agent.19.encounter.slope))
    agent.20.encounter.slope <- append(agent.20.encounter.slope, population[20,1], after = length(agent.20.encounter.slope))
    agent.21.encounter.slope <- append(agent.21.encounter.slope, population[21,1], after = length(agent.21.encounter.slope))
    agent.22.encounter.slope <- append(agent.22.encounter.slope, population[22,1], after = length(agent.22.encounter.slope))
    agent.23.encounter.slope <- append(agent.23.encounter.slope, population[23,1], after = length(agent.23.encounter.slope))
    agent.24.encounter.slope <- append(agent.24.encounter.slope, population[24,1], after = length(agent.24.encounter.slope))
    agent.25.encounter.slope <- append(agent.25.encounter.slope, population[25,1], after = length(agent.25.encounter.slope))
    agent.26.encounter.slope <- append(agent.26.encounter.slope, population[26,1], after = length(agent.26.encounter.slope))
    agent.27.encounter.slope <- append(agent.27.encounter.slope, population[27,1], after = length(agent.27.encounter.slope))
    agent.28.encounter.slope <- append(agent.28.encounter.slope, population[28,1], after = length(agent.28.encounter.slope))
    agent.29.encounter.slope <- append(agent.29.encounter.slope, population[29,1], after = length(agent.29.encounter.slope))
    agent.30.encounter.slope <- append(agent.30.encounter.slope, population[30,1], after = length(agent.30.encounter.slope))
    
    
    
    agent.1.encounter.intercept <- append(agent.1.encounter.intercept, population[1,2], after = length(agent.1.encounter.intercept))
    agent.2.encounter.intercept <- append(agent.2.encounter.intercept, population[2,2], after = length(agent.2.encounter.intercept))
    agent.3.encounter.intercept <- append(agent.3.encounter.intercept, population[3,2], after = length(agent.3.encounter.intercept))
    agent.4.encounter.intercept <- append(agent.4.encounter.intercept, population[4,2], after = length(agent.4.encounter.intercept))
    agent.5.encounter.intercept <- append(agent.5.encounter.intercept, population[5,2], after = length(agent.5.encounter.intercept))
    agent.6.encounter.intercept <- append(agent.6.encounter.intercept, population[6,2], after = length(agent.6.encounter.intercept))
    agent.7.encounter.intercept <- append(agent.7.encounter.intercept, population[7,2], after = length(agent.7.encounter.intercept))
    agent.8.encounter.intercept <- append(agent.8.encounter.intercept, population[8,2], after = length(agent.8.encounter.intercept))
    agent.9.encounter.intercept <- append(agent.9.encounter.intercept, population[9,2], after = length(agent.9.encounter.intercept))
    agent.10.encounter.intercept <- append(agent.10.encounter.intercept, population[10,2], after = length(agent.10.encounter.intercept))
    agent.11.encounter.intercept <- append(agent.11.encounter.intercept, population[11,2], after = length(agent.11.encounter.intercept))
    agent.12.encounter.intercept <- append(agent.12.encounter.intercept, population[12,2], after = length(agent.12.encounter.intercept))
    agent.13.encounter.intercept <- append(agent.13.encounter.intercept, population[13,2], after = length(agent.13.encounter.intercept))
    agent.14.encounter.intercept <- append(agent.14.encounter.intercept, population[14,2], after = length(agent.14.encounter.intercept))
    agent.15.encounter.intercept <- append(agent.15.encounter.intercept, population[15,2], after = length(agent.15.encounter.intercept))
    agent.16.encounter.intercept <- append(agent.16.encounter.intercept, population[16,2], after = length(agent.16.encounter.intercept))
    agent.17.encounter.intercept <- append(agent.17.encounter.intercept, population[17,2], after = length(agent.17.encounter.intercept))
    agent.18.encounter.intercept <- append(agent.18.encounter.intercept, population[18,2], after = length(agent.18.encounter.intercept))
    agent.19.encounter.intercept <- append(agent.19.encounter.intercept, population[19,2], after = length(agent.19.encounter.intercept))
    agent.20.encounter.intercept <- append(agent.20.encounter.intercept, population[20,2], after = length(agent.20.encounter.intercept))
    agent.21.encounter.intercept <- append(agent.21.encounter.intercept, population[21,2], after = length(agent.21.encounter.intercept))
    agent.22.encounter.intercept <- append(agent.22.encounter.intercept, population[22,2], after = length(agent.22.encounter.intercept))
    agent.23.encounter.intercept <- append(agent.23.encounter.intercept, population[23,2], after = length(agent.23.encounter.intercept))
    agent.24.encounter.intercept <- append(agent.24.encounter.intercept, population[24,2], after = length(agent.24.encounter.intercept))
    agent.25.encounter.intercept <- append(agent.25.encounter.intercept, population[25,2], after = length(agent.25.encounter.intercept))
    agent.26.encounter.intercept <- append(agent.26.encounter.intercept, population[26,2], after = length(agent.26.encounter.intercept))
    agent.27.encounter.intercept <- append(agent.27.encounter.intercept, population[27,2], after = length(agent.27.encounter.intercept))
    agent.28.encounter.intercept <- append(agent.28.encounter.intercept, population[28,2], after = length(agent.28.encounter.intercept))
    agent.29.encounter.intercept <- append(agent.29.encounter.intercept, population[29,2], after = length(agent.29.encounter.intercept))
    agent.30.encounter.intercept <- append(agent.30.encounter.intercept, population[30,2], after = length(agent.30.encounter.intercept))
    
    
    agent.1.take.attempt.slope <- append(agent.1.take.attempt.slope, population[1,3], after = length(agent.1.take.attempt.slope))
    agent.2.take.attempt.slope <- append(agent.2.take.attempt.slope, population[2,3], after = length(agent.2.take.attempt.slope))
    agent.3.take.attempt.slope <- append(agent.3.take.attempt.slope, population[3,3], after = length(agent.3.take.attempt.slope))
    agent.4.take.attempt.slope <- append(agent.4.take.attempt.slope, population[4,3], after = length(agent.4.take.attempt.slope))
    agent.5.take.attempt.slope <- append(agent.5.take.attempt.slope, population[5,3], after = length(agent.5.take.attempt.slope))
    agent.6.take.attempt.slope <- append(agent.6.take.attempt.slope, population[6,3], after = length(agent.6.take.attempt.slope))
    agent.7.take.attempt.slope <- append(agent.7.take.attempt.slope, population[7,3], after = length(agent.7.take.attempt.slope))
    agent.8.take.attempt.slope <- append(agent.8.take.attempt.slope, population[8,3], after = length(agent.8.take.attempt.slope))
    agent.9.take.attempt.slope <- append(agent.9.take.attempt.slope, population[9,3], after = length(agent.9.take.attempt.slope))
    agent.10.take.attempt.slope <- append(agent.10.take.attempt.slope, population[10,3], after = length(agent.10.take.attempt.slope))
    agent.11.take.attempt.slope <- append(agent.11.take.attempt.slope, population[11,3], after = length(agent.11.take.attempt.slope))
    agent.12.take.attempt.slope <- append(agent.12.take.attempt.slope, population[12,3], after = length(agent.12.take.attempt.slope))
    agent.13.take.attempt.slope <- append(agent.13.take.attempt.slope, population[13,3], after = length(agent.13.take.attempt.slope))
    agent.14.take.attempt.slope <- append(agent.14.take.attempt.slope, population[14,3], after = length(agent.14.take.attempt.slope))
    agent.15.take.attempt.slope <- append(agent.15.take.attempt.slope, population[15,3], after = length(agent.15.take.attempt.slope))
    agent.16.take.attempt.slope <- append(agent.16.take.attempt.slope, population[16,3], after = length(agent.16.take.attempt.slope))
    agent.17.take.attempt.slope <- append(agent.17.take.attempt.slope, population[17,3], after = length(agent.17.take.attempt.slope))
    agent.18.take.attempt.slope <- append(agent.18.take.attempt.slope, population[18,3], after = length(agent.18.take.attempt.slope))
    agent.19.take.attempt.slope <- append(agent.19.take.attempt.slope, population[19,3], after = length(agent.19.take.attempt.slope))
    agent.20.take.attempt.slope <- append(agent.20.take.attempt.slope, population[20,3], after = length(agent.20.take.attempt.slope))
    agent.21.take.attempt.slope <- append(agent.21.take.attempt.slope, population[21,3], after = length(agent.21.take.attempt.slope))
    agent.22.take.attempt.slope <- append(agent.22.take.attempt.slope, population[22,3], after = length(agent.22.take.attempt.slope))
    agent.23.take.attempt.slope <- append(agent.23.take.attempt.slope, population[23,3], after = length(agent.23.take.attempt.slope))
    agent.24.take.attempt.slope <- append(agent.24.take.attempt.slope, population[24,3], after = length(agent.24.take.attempt.slope))
    agent.25.take.attempt.slope <- append(agent.25.take.attempt.slope, population[25,3], after = length(agent.25.take.attempt.slope))
    agent.26.take.attempt.slope <- append(agent.26.take.attempt.slope, population[26,3], after = length(agent.26.take.attempt.slope))
    agent.27.take.attempt.slope <- append(agent.27.take.attempt.slope, population[27,3], after = length(agent.27.take.attempt.slope))
    agent.28.take.attempt.slope <- append(agent.28.take.attempt.slope, population[28,3], after = length(agent.28.take.attempt.slope))
    agent.29.take.attempt.slope <- append(agent.29.take.attempt.slope, population[29,3], after = length(agent.29.take.attempt.slope))
    agent.30.take.attempt.slope <- append(agent.30.take.attempt.slope, population[30,3], after = length(agent.30.take.attempt.slope))
    
    
    agent.1.take.attempt.intercept <- append(agent.1.take.attempt.intercept, population[1,4], after = length(agent.1.take.attempt.intercept))
    agent.2.take.attempt.intercept <- append(agent.2.take.attempt.intercept, population[2,4], after = length(agent.2.take.attempt.intercept))
    agent.3.take.attempt.intercept <- append(agent.3.take.attempt.intercept, population[3,4], after = length(agent.3.take.attempt.intercept))
    agent.4.take.attempt.intercept <- append(agent.4.take.attempt.intercept, population[4,4], after = length(agent.4.take.attempt.intercept))
    agent.5.take.attempt.intercept <- append(agent.5.take.attempt.intercept, population[5,4], after = length(agent.5.take.attempt.intercept))
    agent.6.take.attempt.intercept <- append(agent.6.take.attempt.intercept, population[6,4], after = length(agent.6.take.attempt.intercept))
    agent.7.take.attempt.intercept <- append(agent.7.take.attempt.intercept, population[7,4], after = length(agent.7.take.attempt.intercept))
    agent.8.take.attempt.intercept <- append(agent.8.take.attempt.intercept, population[8,4], after = length(agent.8.take.attempt.intercept))
    agent.9.take.attempt.intercept <- append(agent.9.take.attempt.intercept, population[9,4], after = length(agent.9.take.attempt.intercept))
    agent.10.take.attempt.intercept <- append(agent.10.take.attempt.intercept, population[10,4], after = length(agent.10.take.attempt.intercept))
    agent.11.take.attempt.intercept <- append(agent.11.take.attempt.intercept, population[11,4], after = length(agent.11.take.attempt.intercept))
    agent.12.take.attempt.intercept <- append(agent.12.take.attempt.intercept, population[12,4], after = length(agent.12.take.attempt.intercept))
    agent.13.take.attempt.intercept <- append(agent.13.take.attempt.intercept, population[13,4], after = length(agent.13.take.attempt.intercept))
    agent.14.take.attempt.intercept <- append(agent.14.take.attempt.intercept, population[14,4], after = length(agent.14.take.attempt.intercept))
    agent.15.take.attempt.intercept <- append(agent.15.take.attempt.intercept, population[15,4], after = length(agent.15.take.attempt.intercept))
    agent.16.take.attempt.intercept <- append(agent.16.take.attempt.intercept, population[16,4], after = length(agent.16.take.attempt.intercept))
    agent.17.take.attempt.intercept <- append(agent.17.take.attempt.intercept, population[17,4], after = length(agent.17.take.attempt.intercept))
    agent.18.take.attempt.intercept <- append(agent.18.take.attempt.intercept, population[18,4], after = length(agent.18.take.attempt.intercept))
    agent.19.take.attempt.intercept <- append(agent.19.take.attempt.intercept, population[19,4], after = length(agent.19.take.attempt.intercept))
    agent.20.take.attempt.intercept <- append(agent.20.take.attempt.intercept, population[20,4], after = length(agent.20.take.attempt.intercept))
    agent.21.take.attempt.intercept <- append(agent.21.take.attempt.intercept, population[21,4], after = length(agent.21.take.attempt.intercept))
    agent.22.take.attempt.intercept <- append(agent.22.take.attempt.intercept, population[22,4], after = length(agent.22.take.attempt.intercept))
    agent.23.take.attempt.intercept <- append(agent.23.take.attempt.intercept, population[23,4], after = length(agent.23.take.attempt.intercept))
    agent.24.take.attempt.intercept <- append(agent.24.take.attempt.intercept, population[24,4], after = length(agent.24.take.attempt.intercept))
    agent.25.take.attempt.intercept <- append(agent.25.take.attempt.intercept, population[25,4], after = length(agent.25.take.attempt.intercept))
    agent.26.take.attempt.intercept <- append(agent.26.take.attempt.intercept, population[26,4], after = length(agent.26.take.attempt.intercept))
    agent.27.take.attempt.intercept <- append(agent.27.take.attempt.intercept, population[27,4], after = length(agent.27.take.attempt.intercept))
    agent.28.take.attempt.intercept <- append(agent.28.take.attempt.intercept, population[28,4], after = length(agent.28.take.attempt.intercept))
    agent.29.take.attempt.intercept <- append(agent.29.take.attempt.intercept, population[29,4], after = length(agent.29.take.attempt.intercept))
    agent.30.take.attempt.intercept <- append(agent.30.take.attempt.intercept, population[30,4], after = length(agent.30.take.attempt.intercept))
    
    
    agent.1.num.losses <- append(agent.1.num.losses, population[1,7], after = length(agent.1.num.losses))
    agent.2.num.losses <- append(agent.2.num.losses, population[2,7], after = length(agent.2.num.losses))
    agent.3.num.losses <- append(agent.3.num.losses, population[3,7], after = length(agent.3.num.losses))
    agent.4.num.losses <- append(agent.4.num.losses, population[4,7], after = length(agent.4.num.losses))
    agent.5.num.losses <- append(agent.5.num.losses, population[5,7], after = length(agent.5.num.losses))
    agent.6.num.losses <- append(agent.6.num.losses, population[6,7], after = length(agent.6.num.losses))
    agent.7.num.losses <- append(agent.7.num.losses, population[7,7], after = length(agent.7.num.losses))
    agent.8.num.losses <- append(agent.8.num.losses, population[8,7], after = length(agent.8.num.losses))
    agent.9.num.losses <- append(agent.9.num.losses, population[9,7], after = length(agent.9.num.losses))
    agent.10.num.losses <- append(agent.10.num.losses, population[10,7], after = length(agent.10.num.losses))
    agent.11.num.losses <- append(agent.11.num.losses, population[11,7], after = length(agent.11.num.losses))
    agent.12.num.losses <- append(agent.12.num.losses, population[12,7], after = length(agent.12.num.losses))
    agent.13.num.losses <- append(agent.13.num.losses, population[13,7], after = length(agent.13.num.losses))
    agent.14.num.losses <- append(agent.14.num.losses, population[14,7], after = length(agent.14.num.losses))
    agent.15.num.losses <- append(agent.15.num.losses, population[15,7], after = length(agent.15.num.losses))
    agent.16.num.losses <- append(agent.16.num.losses, population[16,7], after = length(agent.16.num.losses))
    agent.17.num.losses <- append(agent.17.num.losses, population[17,7], after = length(agent.17.num.losses))
    agent.18.num.losses <- append(agent.18.num.losses, population[18,7], after = length(agent.18.num.losses))
    agent.19.num.losses <- append(agent.19.num.losses, population[19,7], after = length(agent.19.num.losses))
    agent.20.num.losses <- append(agent.20.num.losses, population[20,7], after = length(agent.20.num.losses))
    agent.21.num.losses <- append(agent.21.num.losses, population[21,7], after = length(agent.21.num.losses))
    agent.22.num.losses <- append(agent.22.num.losses, population[22,7], after = length(agent.22.num.losses))
    agent.23.num.losses <- append(agent.23.num.losses, population[23,7], after = length(agent.23.num.losses))
    agent.24.num.losses <- append(agent.24.num.losses, population[24,7], after = length(agent.24.num.losses))
    agent.25.num.losses <- append(agent.25.num.losses, population[25,7], after = length(agent.25.num.losses))
    agent.26.num.losses <- append(agent.26.num.losses, population[26,7], after = length(agent.26.num.losses))
    agent.27.num.losses <- append(agent.27.num.losses, population[27,7], after = length(agent.27.num.losses))
    agent.28.num.losses <- append(agent.28.num.losses, population[28,7], after = length(agent.28.num.losses))
    agent.29.num.losses <- append(agent.29.num.losses, population[29,7], after = length(agent.29.num.losses))
    agent.30.num.losses <- append(agent.30.num.losses, population[30,7], after = length(agent.30.num.losses))
    
    population <- genetic.algorithm.3(population)
  }
  return(list(a = population, b = average.encounter.slope, c = average.encounter.intercept, d = average.take.attempt.slope, 
              e = average.take.attempt.intercept, f = median.num.losses, 
              g = matrix(c(agent.1.encounter.slope, agent.2.encounter.slope, agent.3.encounter.slope, agent.4.encounter.slope, agent.5.encounter.slope,
                           agent.6.encounter.slope, agent.7.encounter.slope, agent.8.encounter.slope, agent.9.encounter.slope, agent.10.encounter.slope,
                           agent.11.encounter.slope, agent.12.encounter.slope, agent.13.encounter.slope, agent.14.encounter.slope, agent.15.encounter.slope,
                           agent.16.encounter.slope, agent.17.encounter.slope, agent.18.encounter.slope, agent.19.encounter.slope, agent.20.encounter.slope,
                           agent.21.encounter.slope, agent.22.encounter.slope, agent.23.encounter.slope, agent.24.encounter.slope, agent.25.encounter.slope,
                           agent.26.encounter.slope, agent.27.encounter.slope, agent.28.encounter.slope, agent.29.encounter.slope, agent.30.encounter.slope),
                         byrow = TRUE, nrow = 30), 
              h = matrix(c(agent.1.encounter.intercept, agent.2.encounter.intercept, agent.3.encounter.intercept, agent.4.encounter.intercept, agent.5.encounter.intercept,
                           agent.6.encounter.intercept, agent.7.encounter.intercept, agent.8.encounter.intercept, agent.9.encounter.intercept, agent.10.encounter.intercept,
                           agent.11.encounter.intercept, agent.12.encounter.intercept, agent.13.encounter.intercept, agent.14.encounter.intercept, agent.15.encounter.intercept,
                           agent.16.encounter.intercept, agent.17.encounter.intercept, agent.18.encounter.intercept, agent.19.encounter.intercept, agent.20.encounter.intercept,
                           agent.21.encounter.intercept, agent.22.encounter.intercept, agent.23.encounter.intercept, agent.24.encounter.intercept, agent.25.encounter.intercept,
                           agent.26.encounter.intercept, agent.27.encounter.intercept, agent.28.encounter.intercept, agent.29.encounter.intercept, agent.30.encounter.intercept),
                         byrow = TRUE, nrow = 30), 
              i = matrix(c(agent.1.take.attempt.slope, agent.2.take.attempt.slope, agent.3.take.attempt.slope, agent.4.take.attempt.slope, agent.5.take.attempt.slope,
                           agent.6.take.attempt.slope, agent.7.take.attempt.slope, agent.8.take.attempt.slope, agent.9.take.attempt.slope, agent.10.take.attempt.slope,
                           agent.11.take.attempt.slope, agent.12.take.attempt.slope, agent.13.take.attempt.slope, agent.14.take.attempt.slope, agent.15.take.attempt.slope,
                           agent.16.take.attempt.slope, agent.17.take.attempt.slope, agent.18.take.attempt.slope, agent.19.take.attempt.slope, agent.20.take.attempt.slope,
                           agent.21.take.attempt.slope, agent.22.take.attempt.slope, agent.23.take.attempt.slope, agent.24.take.attempt.slope, agent.25.take.attempt.slope,
                           agent.26.take.attempt.slope, agent.27.take.attempt.slope, agent.28.take.attempt.slope, agent.29.take.attempt.slope, agent.30.take.attempt.slope),
                         byrow = TRUE, nrow = 30), 
              j = matrix(c(agent.1.take.attempt.intercept, agent.2.take.attempt.intercept, agent.3.take.attempt.intercept, agent.4.take.attempt.intercept, agent.5.take.attempt.intercept,
                           agent.6.take.attempt.intercept, agent.7.take.attempt.intercept, agent.8.take.attempt.intercept, agent.9.take.attempt.intercept, agent.10.take.attempt.intercept,
                           agent.11.take.attempt.intercept, agent.12.take.attempt.intercept, agent.13.take.attempt.intercept, agent.14.take.attempt.intercept, agent.15.take.attempt.intercept,
                           agent.16.take.attempt.intercept, agent.17.take.attempt.intercept, agent.18.take.attempt.intercept, agent.19.take.attempt.intercept, agent.20.take.attempt.intercept,
                           agent.21.take.attempt.intercept, agent.22.take.attempt.intercept, agent.23.take.attempt.intercept, agent.24.take.attempt.intercept, agent.25.take.attempt.intercept,
                           agent.26.take.attempt.intercept, agent.27.take.attempt.intercept, agent.28.take.attempt.intercept, agent.29.take.attempt.intercept, agent.30.take.attempt.intercept),
                         byrow = TRUE, nrow = 30),
              k = matrix(c(agent.1.num.losses, agent.2.num.losses, agent.3.num.losses, agent.4.num.losses, agent.5.num.losses,
                           agent.6.num.losses, agent.7.num.losses, agent.8.num.losses, agent.9.num.losses, agent.10.num.losses,
                           agent.11.num.losses, agent.12.num.losses, agent.13.num.losses, agent.14.num.losses, agent.15.num.losses,
                           agent.16.num.losses, agent.17.num.losses, agent.18.num.losses, agent.19.num.losses, agent.20.num.losses,
                           agent.21.num.losses, agent.22.num.losses, agent.23.num.losses, agent.24.num.losses, agent.25.num.losses,
                           agent.26.num.losses, agent.27.num.losses, agent.28.num.losses, agent.29.num.losses, agent.30.num.losses),
                         byrow = TRUE, nrow = 30)))
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
results <- run.n.gens.3(2000, population)

population <- results$a
average.encounter.slope <- results$b
average.encounter.intercept <- results$c
average.take.attempt.slope <- results$d
average.take.attempt.intercept <- results$e
median.num.losses <- results$f
encounter.slope.matrix <- results$g
encounter.intercept.matrix <- results$h
take.attempt.slope.matrix <- results$i
take.attempt.intercept.matrix <- results$j
num.losses.matrix <- results$k


plot(1:2000, average.encounter.slope, xlab = "Selection Number", ylab = "M1" , main = "Average M1 After Selection Process")
plot(1:2000, average.encounter.intercept, xlab = "Selection Number", ylab = "B1" , main = "Average B1 After Selection Process")
plot(1:2000, average.take.attempt.slope, xlab = "Selection Number", ylab = "M2" , main = "Average M2 After Selection Process")
plot(1:2000, average.take.attempt.intercept, xlab = "Selection Number", ylab = "B2" , main = "Average B2 After Selection Process")
plot(1:2000, median.num.losses, xlab = "Selection Number", ylab = "Num Losses", main = "Median Number of Losses After Selection Process")

plot(1:10, agent.1.encounter.slope)
par(new = TRUE)
plot(1:10, agent.1.encounter.intercept, col = "green")
par(new = TRUE)
plot(1:10, agent.1.take.attempt.slope, col = "blue")
par(new = TRUE)
plot(1:10, agent.1.take.attempt.intercept, col = "red")
par(new = TRUE)
plot(1:10, agent.1.num.losses)


plot(1:2000, encounter.slope.matrix[1,])
plot(1:2000, encounter.slope.matrix[2,])
plot(1:2000, encounter.slope.matrix[3,])
plot(1:2000, encounter.slope.matrix[4,])
plot(1:2000, encounter.slope.matrix[5,])
plot(1:2000, encounter.slope.matrix[6,])
plot(1:2000, encounter.slope.matrix[7,])
plot(1:2000, encounter.slope.matrix[8,])
plot(1:2000, encounter.slope.matrix[9,])
plot(1:2000, encounter.slope.matrix[10,])
plot(1:2000, encounter.slope.matrix[11,])
plot(1:2000, encounter.slope.matrix[12,])
plot(1:2000, encounter.slope.matrix[13,])
plot(1:2000, encounter.slope.matrix[14,])
plot(1:2000, encounter.slope.matrix[15,])
plot(1:2000, encounter.slope.matrix[16,])
plot(1:2000, encounter.slope.matrix[17,])
plot(1:2000, encounter.slope.matrix[18,])
plot(1:2000, encounter.slope.matrix[19,])
plot(1:2000, encounter.slope.matrix[20,])
plot(1:2000, encounter.slope.matrix[21,])
plot(1:2000, encounter.slope.matrix[22,])
plot(1:2000, encounter.slope.matrix[23,])
plot(1:2000, encounter.slope.matrix[24,])
plot(1:2000, encounter.slope.matrix[25,])
plot(1:2000, encounter.slope.matrix[26,])
plot(1:2000, encounter.slope.matrix[27,])
plot(1:2000, encounter.slope.matrix[28,])
plot(1:2000, encounter.slope.matrix[29,])
plot(1:2000, encounter.slope.matrix[30,])

plot(1:2000, encounter.intercept.matrix[1,])
plot(1:2000, encounter.intercept.matrix[2,])
plot(1:2000, encounter.intercept.matrix[3,])
plot(1:2000, encounter.intercept.matrix[4,])
plot(1:2000, encounter.intercept.matrix[5,])
plot(1:2000, encounter.intercept.matrix[6,])
plot(1:2000, encounter.intercept.matrix[7,])
plot(1:2000, encounter.intercept.matrix[8,])
plot(1:2000, encounter.intercept.matrix[9,])
plot(1:2000, encounter.intercept.matrix[10,])
plot(1:2000, encounter.intercept.matrix[11,])
plot(1:2000, encounter.intercept.matrix[12,])
plot(1:2000, encounter.intercept.matrix[13,])
plot(1:2000, encounter.intercept.matrix[14,])
plot(1:2000, encounter.intercept.matrix[15,])
plot(1:2000, encounter.intercept.matrix[16,])
plot(1:2000, encounter.intercept.matrix[17,])
plot(1:2000, encounter.intercept.matrix[18,])
plot(1:2000, encounter.intercept.matrix[19,])
plot(1:2000, encounter.intercept.matrix[20,])
plot(1:2000, encounter.intercept.matrix[21,])
plot(1:2000, encounter.intercept.matrix[22,])
plot(1:2000, encounter.intercept.matrix[23,])
plot(1:2000, encounter.intercept.matrix[24,])
plot(1:2000, encounter.intercept.matrix[25,])
plot(1:2000, encounter.intercept.matrix[26,])
plot(1:2000, encounter.intercept.matrix[27,])
plot(1:2000, encounter.intercept.matrix[28,])
plot(1:2000, encounter.intercept.matrix[29,])
plot(1:2000, encounter.intercept.matrix[30,])

plot(1:2000, take.attempt.slope.matrix[1,])
plot(1:2000, take.attempt.slope.matrix[2,])
plot(1:2000, take.attempt.slope.matrix[3,])
plot(1:2000, take.attempt.slope.matrix[4,])
plot(1:2000, take.attempt.slope.matrix[5,])
plot(1:2000, take.attempt.slope.matrix[6,])
plot(1:2000, take.attempt.slope.matrix[7,])
plot(1:2000, take.attempt.slope.matrix[8,])
plot(1:2000, take.attempt.slope.matrix[9,])
plot(1:2000, take.attempt.slope.matrix[10,])
plot(1:2000, take.attempt.slope.matrix[11,])
plot(1:2000, take.attempt.slope.matrix[12,])
plot(1:2000, take.attempt.slope.matrix[13,])
plot(1:2000, take.attempt.slope.matrix[14,])
plot(1:2000, take.attempt.slope.matrix[15,])
plot(1:2000, take.attempt.slope.matrix[16,])
plot(1:2000, take.attempt.slope.matrix[17,])
plot(1:2000, take.attempt.slope.matrix[18,])
plot(1:2000, take.attempt.slope.matrix[19,])
plot(1:2000, take.attempt.slope.matrix[20,])
plot(1:2000, take.attempt.slope.matrix[21,])
plot(1:2000, take.attempt.slope.matrix[22,])
plot(1:2000, take.attempt.slope.matrix[23,])
plot(1:2000, take.attempt.slope.matrix[24,])
plot(1:2000, take.attempt.slope.matrix[25,])
plot(1:2000, take.attempt.slope.matrix[26,])
plot(1:2000, take.attempt.slope.matrix[27,])
plot(1:2000, take.attempt.slope.matrix[28,])
plot(1:2000, take.attempt.slope.matrix[29,])
plot(1:2000, take.attempt.slope.matrix[30,])

plot(1:2000, take.attempt.intercept.matrix[1,])
plot(1:2000, take.attempt.intercept.matrix[2,])
plot(1:2000, take.attempt.intercept.matrix[3,])
plot(1:2000, take.attempt.intercept.matrix[4,])
plot(1:2000, take.attempt.intercept.matrix[5,])
plot(1:2000, take.attempt.intercept.matrix[6,])
plot(1:2000, take.attempt.intercept.matrix[7,])
plot(1:2000, take.attempt.intercept.matrix[8,])
plot(1:2000, take.attempt.intercept.matrix[9,])
plot(1:2000, take.attempt.intercept.matrix[10,])
plot(1:2000, take.attempt.intercept.matrix[11,])
plot(1:2000, take.attempt.intercept.matrix[12,])
plot(1:2000, take.attempt.intercept.matrix[13,])
plot(1:2000, take.attempt.intercept.matrix[14,])
plot(1:2000, take.attempt.intercept.matrix[15,])
plot(1:2000, take.attempt.intercept.matrix[16,])
plot(1:2000, take.attempt.intercept.matrix[17,])
plot(1:2000, take.attempt.intercept.matrix[18,])
plot(1:2000, take.attempt.intercept.matrix[19,])
plot(1:2000, take.attempt.intercept.matrix[20,])
plot(1:2000, take.attempt.intercept.matrix[21,])
plot(1:2000, take.attempt.intercept.matrix[22,])
plot(1:2000, take.attempt.intercept.matrix[23,])
plot(1:2000, take.attempt.intercept.matrix[24,])
plot(1:2000, take.attempt.intercept.matrix[25,])
plot(1:2000, take.attempt.intercept.matrix[26,])
plot(1:2000, take.attempt.intercept.matrix[27,])
plot(1:2000, take.attempt.intercept.matrix[28,])
plot(1:2000, take.attempt.intercept.matrix[29,])
plot(1:2000, take.attempt.intercept.matrix[30,])

plot(1:2000, num.losses.matrix[1,])
plot(1:2000, num.losses.matrix[2,])
plot(1:2000, num.losses.matrix[3,])
plot(1:2000, num.losses.matrix[4,])
plot(1:2000, num.losses.matrix[5,])
plot(1:2000, num.losses.matrix[6,])
plot(1:2000, num.losses.matrix[7,])
plot(1:2000, num.losses.matrix[8,])
plot(1:2000, num.losses.matrix[9,])
plot(1:2000, num.losses.matrix[10,])
plot(1:2000, num.losses.matrix[11,])
plot(1:2000, num.losses.matrix[12,])
plot(1:2000, num.losses.matrix[13,])
plot(1:2000, num.losses.matrix[14,])
plot(1:2000, num.losses.matrix[15,])
plot(1:2000, num.losses.matrix[16,])
plot(1:2000, num.losses.matrix[17,])
plot(1:2000, num.losses.matrix[18,])
plot(1:2000, num.losses.matrix[19,])
plot(1:2000, num.losses.matrix[20,])
plot(1:2000, num.losses.matrix[21,])
plot(1:2000, num.losses.matrix[22,])
plot(1:2000, num.losses.matrix[23,])
plot(1:2000, num.losses.matrix[24,])
plot(1:2000, num.losses.matrix[25,])
plot(1:2000, num.losses.matrix[26,])
plot(1:2000, num.losses.matrix[27,])
plot(1:2000, num.losses.matrix[28,])
plot(1:2000, num.losses.matrix[29,])
plot(1:2000, num.losses.matrix[30,])



x <- 1:500
plot(x,agent.1.encounter.slope, col="red")
par(new = TRUE)
plot(x,agent.1.num.losses, col="blue")

plot(average.encounter.slope, average.encounter.intercept)
plot(average.take.attempt.slope, average.take.attempt.intercept)


qplot(1:10, average.encounter.slope) +
  theme(legend.position = "none") +
  #scale_color_gradient(low = "yellow", high = "red") +
  geom_point(color = 'gold2') +
  xlab("Selection Number") +
  ylab("M1") +
  ggtitle("Average M1 After Selection Process")

mean(average.encounter.slope[-(1:180)])
mean(average.encounter.intercept[-(1:180)])
mean(average.take.attempt.slope[-(1:180)])
mean(average.take.attempt.intercept[-(1:180)])

print(population)

num.losses.matrix
  