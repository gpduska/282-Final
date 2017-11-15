# Write model here

# each person has a certain number of perameters, each person has a power score. The power scores are broken down into 4 sub scores. each person has a cetain
# algorithm that they are using to enforce a particular power structure. These sort of correlate to ideologies. Examples of these algorithms would be one 
# where everyone is aiming to put/keep everyone else at a score of 1 powerScore. Another algorithm would be to try to maximize the most power for yourself.
# everyone's power score is influenced by their 5 (arbitrary) closest contacts. A radical action could then be defined as either applying an algorithem that
# would decrease the majority of peoples power, or relativistikly as one that is different from your own. The degree that a persons algorithm is applied to 
# the rest of the population depends on how powerful a person is. 

# I am going to use a genetic algorithm to simulate peoples changing ideologies. I will need to simulate peoples slight variations in their algorthims (functions)
# and also variation that includes the abbility to change the base function of the algorithm. There wouldn't nessesarily need to be a selection process
# because certain power structures would become stabalized. One such example of a relatively stable power structure is one generally in a x^2 shape. The
# people at the top of the power structure would have the most impact on the power of everyone else. They would have enough impact to keep their prefered
# power structure in place. Especially since they wouldnt be working alone. Another stable power structure would be the one where everyone is enforcing 
# that everyone is at a power of 1. There are probably others as well. 

# there would actually be ideology functions for each person, that worked on 4 different identity powerScores. The genetic ideology mutation could be random;
# If that were the case, then there would need to be some sort of pruning mechanism that allowed people to switch ideologies. What makes most sense to me,
# is that the more conntact you have with people holding different algorithms, the more likely you are to switch your algorithm to theirs. Say, if 4 out of
# your 5 closest contacts have a certain algorithm, you become very likely to adopt their algorithm. Ideally, each person could be simulated by a neural net
# where each person was trying to fit the population to a certain function. When a person switches ideologies (functions), their neural net gets retrained.
# Each position in person A's target vector would mutate towards the average of the correlating positions in the target vecotrs of person A's 5 closest
# contacts. A person's overall power score would still dicate how strongly their power function is applied. 

# In the first round, everyone has 1 point to distribute to everyone else. They distribute to everyone else how they see fit (based on their target vector)
# and then receive power from everone else all in one round. The next round they have some power value that is not 1, during this next round, players 
# continue to distribute power as they see fit, according to their 4 identity functions. This time, the magnitude of their distribution is greater 
# due to the fact theat they have more power to distribute. 

# iTarget1 is a vector of length n.population. In this model, all of the elements of iTarget1 add up to 0.25. Every element of iTarget1 is between 0 and 1.
# Every other iTarget vector has the same perameters. The iTarget vectors represent how each person to whom they belong assigns power to other people. 
# When assigning power to other people, the real target numbers that are used is iTarget1 * overallPowerScore


# the first four perameters, the identities, are a number between 0 and one. These are not measures of power, rather, they are a parameter of a measurable
# performance. People applly four different power functions acorss these identity parameters. They can look like x^2, 1/x, or some sort of bell curve
# (anything really). The overall power score is then

# the iTargets are vectors that tell a person how to distribute power based on other peoples iMeasures. A persons neural net changes a persons iTargets
# in order to acheive their over

# person <- c(overallPowerScore, iScore1, iScore2, iScore3, iScore4, overallPowerRank, iRank1, iRank2, iRank3, iRank4,
#             iTarget1, iTarget2, iTarget3, iTarget4, iMeasure1, iMeasure2, iMeasure3, iMeasure4) 

# Every person should have a powerScore goal. The greedy goal would be to maximize your own power. My idea is to have a neural net for each person that 
# is figuring out how to distribute power in such a way that brings them closer to their goal. 
library(dplyr)

n.population <- 300  #size of population

# these groups are sort of arbitrarily defiened depending on the system that the model is describing
n.groupA <- n.population * 0.01
n.groupB <- n.population * 0.04
n.groupC <- n.population * 0.17
n.groupD <- n.population * 0.17
n.groupE <- n.population * (1 - (0.01 + 0.04 + 0.17 + 0.17))

# initial powerScore for each member of their respective group
groupA.power.member <- 5
groupB.power.member <- 4
groupC.power.member <- 3
groupD.power.member <- 2
groupE.power.member <- 1

# total amount of power within each group identity
groupA.power.total <- groupA.power.member * n.groupA
groupB.power.total <- groupB.power.member * n.groupB
groupC.power.total <- groupC.power.member * n.groupC
groupD.power.total <- groupD.power.member * n.groupD
groupE.power.total <- groupE.power.member * n.groupE

# total amount of power within the system
system.total.power <- groupA.power.total + groupB.power.total + groupC.power.total + groupD.power.total + groupE.power.total

# the number that a random number between 0 and 1 has to be less than in order for an encounter to be initiated
prob.A.to.A.encounter <- 0.005
prob.A.to.B.encounter <- 0.005
prob.A.to.C.encounter <- 0.005
prob.A.to.D.encounter <- 0.005
prob.A.to.E.encounter <- 0.005

prob.B.to.A.encounter <- 0.005
prob.B.to.B.encounter <- 0.005
prob.B.to.C.encounter <- 0.005
prob.B.to.D.encounter <- 0.005
prob.B.to.E.encounter <- 0.005

prob.C.to.A.encounter <- 0.005
prob.C.to.B.encounter <- 0.005
prob.C.to.C.encounter <- 0.005
prob.C.to.D.encounter <- 0.005
prob.C.to.E.encounter <- 0.005

prob.D.to.A.encounter <- 0.005
prob.D.to.B.encounter <- 0.005
prob.D.to.C.encounter <- 0.005
prob.D.to.D.encounter <- 0.005
prob.D.to.E.encounter <- 0.005

prob.E.to.A.encounter <- 0.005
prob.E.to.B.encounter <- 0.005
prob.E.to.C.encounter <- 0.005
prob.E.to.D.encounter <- 0.005
prob.E.to.E.encounter <- 0.005

# the number that a random number between 0 and 1 has to be less than in order for an ask of power to happen
prob.A.to.A.ask <- 0.5
prob.A.to.B.ask <- 0.5
prob.A.to.C.ask <- 0.5
prob.A.to.D.ask <- 0.5
prob.A.to.E.ask <- 0.5

prob.B.to.A.ask <- 0.5
prob.B.to.B.ask <- 0.5
prob.B.to.C.ask <- 0.5
prob.B.to.D.ask <- 0.5
prob.B.to.E.ask <- 0.5

prob.C.to.A.ask <- 0.5
prob.C.to.B.ask <- 0.5
prob.C.to.C.ask <- 0.5
prob.C.to.D.ask <- 0.5
prob.C.to.E.ask <- 0.5

prob.D.to.A.ask <- 0.5
prob.D.to.B.ask <- 0.5
prob.D.to.C.ask <- 0.5
prob.D.to.D.ask <- 0.5
prob.D.to.E.ask <- 0.5

prob.E.to.A.ask <- 0.5
prob.E.to.B.ask <- 0.5
prob.E.to.C.ask <- 0.5
prob.E.to.D.ask <- 0.5
prob.E.to.E.ask <- 0.5

# the standard deviation of the normal curves that are used to decide how much a person takes from another based on group
A.to.A.take.sd <- 1
A.to.B.take.sd <- 1
A.to.C.take.sd <- 1
A.to.D.take.sd <- 1
A.to.E.take.sd <- 1

B.to.A.take.sd <- 1
B.to.B.take.sd <- 1
B.to.C.take.sd <- 1
B.to.D.take.sd <- 1
B.to.E.take.sd <- 1

C.to.A.take.sd <- 1
C.to.B.take.sd <- 1
C.to.C.take.sd <- 1
C.to.D.take.sd <- 1
C.to.E.take.sd <- 1

D.to.A.take.sd <- 1
D.to.B.take.sd <- 1
D.to.C.take.sd <- 1
D.to.D.take.sd <- 1
D.to.E.take.sd <- 1

E.to.A.take.sd <- 1
E.to.B.take.sd <- 1
E.to.C.take.sd <- 1
E.to.D.take.sd <- 1
E.to.E.take.sd <- 1

# the standard deviation of the normal curves that are used to decide how much a person asks from another based on group
A.to.A.ask.sd <- 1
A.to.B.ask.sd <- 1
A.to.C.ask.sd <- 1
A.to.D.ask.sd <- 1
A.to.E.ask.sd <- 1

B.to.A.ask.sd <- 1
B.to.B.ask.sd <- 1
B.to.C.ask.sd <- 1
B.to.D.ask.sd <- 1
B.to.E.ask.sd <- 1

C.to.A.ask.sd <- 1
C.to.B.ask.sd <- 1
C.to.C.ask.sd <- 1
C.to.D.ask.sd <- 1
C.to.E.ask.sd <- 1

D.to.A.ask.sd <- 1
D.to.B.ask.sd <- 1
D.to.C.ask.sd <- 1
D.to.D.ask.sd <- 1
D.to.E.ask.sd <- 1

E.to.A.ask.sd <- 1
E.to.B.ask.sd <- 1
E.to.C.ask.sd <- 1
E.to.D.ask.sd <- 1
E.to.E.ask.sd <- 1

# the proprtion of their own power that each group member is willing to give to all members of each other group  
prop.A.to.A.give <- 0.4
prop.A.to.B.give <- 0.2
prop.A.to.C.give <- 0.2
prop.A.to.D.give <- 0.15
prop.A.to.E.give <- 0.05

prop.B.to.A.give <- 0.4
prop.B.to.B.give <- 0.2
prop.B.to.C.give <- 0.2
prop.B.to.D.give <- 0.15
prop.B.to.E.give <- 0.05

prop.C.to.A.give <- 0.4
prop.C.to.B.give <- 0.2
prop.C.to.C.give <- 0.2
prop.C.to.D.give <- 0.15
prop.C.to.E.give <- 0.05

prop.D.to.A.give <- 0.4
prop.D.to.B.give <- 0.2
prop.D.to.C.give <- 0.2
prop.D.to.D.give <- 0.15
prop.D.to.E.give <- 0.05

prop.E.to.A.give <- 0.4
prop.E.to.B.give <- 0.2
prop.E.to.C.give <- 0.2
prop.E.to.D.give <- 0.15
prop.E.to.E.give <- 0.05

# this is how much less than the group average power score a person within a particular group has be in order to have their perameters changed to mimic the most powerful person in the group
group.A.sd.selection <- 2
group.B.sd.selection <- 1.5
group.C.sd.selection <- 1.5
group.D.sd.selection <- 1
group.E.sd.selection <- 0.5

# this is how much more than the group averag power score a person within a particular group has be in order to have their perameters not mutated
group.A.sd.mutation <- 2
group.B.sd.mutation <- 1.5
group.C.sd.mutation <- 1.5
group.D.sd.mutation <- 1
group.E.sd.mutation <- 0.5

# this is the standard deviation of how much the individu peramters are mutated by
mutation.sd <- 0.01

# This function instantiates the origional matrix
initial.population.v1 <- function(){
  
  name.vector <- 1:n.population
  
  group.vector <- c(rep(0.01, n.groupA), rep(0.02, n.groupB), rep(0.03, n.groupC), rep(0.04, n.groupD), rep(0.05, n.groupE))
  
  power.vector <- c(rep(groupA.power.member, n.groupA), rep(groupB.power.member, n.groupB), rep(groupC.power.member, n.groupC),
                    rep(groupD.power.member, n.groupD), rep(groupE.power.member, n.groupE))
  
  
  return(matrix(c(name.vector, group.vector, power.vector), byrow=F, nrow = n.population))
}


# This function simulates one round of every person exhangeing power with every other person
exchange.power.v1 <- function(population){
  
  pop.vector <- 1:n.population
  
  index.initiated <- c()
  index.initiator <- c()
  exchange.type <- c()
  exchange.amount <- c()
  group.initiated <- c()
  group.initiator <- c()
  
  # for each person in the population...
  for(i in pop.vector){
    
    # check to see if person i is in group A
    if(population[i, 2] == 0.01){
      
      # go through each other person in the population...
      for(j in pop.vector[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.A.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.A.to.A.ask){
              
              powerAsk <- abs(rnorm(1, 0, A.to.A.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.A.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group B
        } else if(population[j,2] == 0.02){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.A.to.B.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.A.to.B.ask){
              
              powerAsk <- abs(rnorm(1, 0, A.to.B.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.B.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group C
        } else if(population[j,2] == 0.03){
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.A.to.C.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.A.to.C.ask){
              
              powerAsk <- abs(rnorm(1, 0, A.to.C.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.C.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group D
        } else if(population[j,2] == 0.04){
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.A.to.D.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.A.to.D.ask){
              
              powerAsk <- abs(rnorm(1, 0, A.to.D.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.D.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group E
        } else if(population[j,2] == 0.05){
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.A.to.E.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.A.to.E.ask){
              
              powerAsk <- abs(rnorm(1, 0, A.to.E.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.E.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
        }
      }
      
      #check to see if person i is in group B
    } else if(population[i, 2] == 0.02){
      
      # go through each other person in the population...
      for(j in pop.vector[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.B.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.B.to.A.ask){
              
              powerAsk <- abs(rnorm(1, 0, B.to.A.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.A.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group B
        } else if(population[j,2] == 0.02){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.B.to.B.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.B.to.B.ask){
              
              powerAsk <- abs(rnorm(1, 0, B.to.B.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.B.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group C
        } else if(population[j,2] == 0.03){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.B.to.C.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.B.to.C.ask){
              
              powerAsk <- abs(rnorm(1, 0, B.to.C.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.C.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group D
        } else if(population[j,2] == 0.04){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.B.to.D.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.B.to.D.ask){
              
              powerAsk <- abs(rnorm(1, 0, B.to.D.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.D.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group E
        } else if(population[j,2] == 0.05){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.B.to.E.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.B.to.E.ask){
              
              powerAsk <- abs(rnorm(1, 0, B.to.E.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.E.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
        }
      }
      
      #check to see if person i is in group C
    } else if(population[i, 2] == 0.03){
      
      # go through each other person in the population...
      for(j in pop.vector[-i]){
        
        # check to see if person is j in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.C.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.C.to.A.ask){
              
              powerAsk <- abs(rnorm(1, 0, C.to.A.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.A.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group B
        } else if(population[j,2] == 0.02){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.C.to.B.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.C.to.B.ask){
              
              powerAsk <- abs(rnorm(1, 0, C.to.B.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.B.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group C
        } else if(population[j,2] == 0.03){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.C.to.C.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.C.to.C.ask){
              
              powerAsk <- abs(rnorm(1, 0, C.to.C.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.C.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group D
        } else if(population[j,2] == 0.04){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.C.to.D.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.C.to.D.ask){
              
              powerAsk <- abs(rnorm(1, 0, C.to.D.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.D.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group E
        } else if(population[j,2] == 0.05){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.C.to.E.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.C.to.E.ask){
              
              powerAsk <- abs(rnorm(1, 0, C.to.E.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.E.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
        }
      }
      
      #check to see if person i is in group D
    } else if(population[i, 2] == 0.04){
      
      # go through each other person in the population...
      for(j in pop.vector[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.D.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.D.to.A.ask){
              
              powerAsk <- abs(rnorm(1, 0, D.to.A.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.A.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group B
        } else if(population[j,2] == 0.02){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.D.to.B.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.D.to.B.ask){
              
              powerAsk <- abs(rnorm(1, 0, D.to.B.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.B.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group C
        } else if(population[j,2] == 0.03){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.D.to.C.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.D.to.C.ask){
              
              powerAsk <- abs(rnorm(1, 0, D.to.C.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.C.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group D
        } else if(population[j,2] == 0.04){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.D.to.D.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.D.to.D.ask){
              
              powerAsk <- abs(rnorm(1, 0, D.to.D.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.D.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group E
        } else if(population[j,2] == 0.05){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.D.to.E.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.D.to.E.ask){
              
              powerAsk <- abs(rnorm(1, 0, D.to.E.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.E.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
        }
      }
      
      #check to see if person i is in group E
    } else if(population[i, 2] == 0.05){
      
      # go through each other person in the population...
      for(j in pop.vector[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.E.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.E.to.A.ask){
              
              powerAsk <- abs(rnorm(1, 0, E.to.A.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.A.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group B
        } else if(population[j,2] == 0.02){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.E.to.B.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.E.to.B.ask){
              
              powerAsk <- abs(rnorm(1, 0, E.to.B.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.B.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group C
        } else if(population[j,2] == 0.03){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.E.to.C.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.E.to.C.ask){
              
              powerAsk <- abs(rnorm(1, 0, E.to.C.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.C.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group D
        } else if(population[j,2] == 0.04){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.E.to.D.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.E.to.D.ask){
              
              powerAsk <- abs(rnorm(1, 0, E.to.D.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.D.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
          
          # check to see if person j is in group E
        } else if(population[j,2] == 0.05){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.E.to.E.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.E.to.E.ask){
              
              powerAsk <- abs(rnorm(1, 0, E.to.E.ask.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.E.take.sd))
              
              index.initiated <- append(index.initiated, j, after = length(index.initiated))
              index.initiator <- append(index.initiator, i, after = length(index.initiator))
              exchange.type <- append(exchange.type, "take", after = length(exchange.type))
              exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
              group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
              group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            }
          }
        }
      }
    }
  }
  
  # This creates a data frame that will be used on the second and third steps of teh power exchange when people resond to people taking and asking
  # for their power. 
  return(data.frame(index.initiated, index.initiator, exchange.type, exchange.amount, group.initiated, group.initiator))
  
}

exchange.response.v1 <- function(intermediate.exchange.data){
  
  # response for the "takes"
  for(i in 1:n.population){
    
    total.attempted.take <- sum((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))$exchange.amount)
    
    if((0 < total.attempted.take) & (total.attempted.take < population[i,3])){
      
      # each person attempting to take power from person i gives as much power they were attempting to take to person i
      for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[,1])){
        
        power.take <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,4]
        index.loser <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,2]
        
        population[index.loser, 3] <- population[index.loser, 3] - power.take
        population[i,3] <- population[i,3] + power.take
      }
    } else if((total.attempted.take > population[i,3]) & (total.attempted.take > 0)) {
      
      for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[,1])){
        
        power.take <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,4]
        index.winner <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,2]
        
        population[index.winner, 3] <- population[index.winner, 3] + power.take
        
        population[i,3] <- population[i,3] - power.take
      }
    }
  }
  
  # response for the "asks"
  for(i in 1:n.population){
    
    # only give out power if your power is greater than 0!
    if(population[i,3] > 0){
      
      
      total.ask.group.A <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.01))$exchange.amount)
      
      total.ask.group.B <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.02))$exchange.amount)
      
      total.ask.group.C <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.03))$exchange.amount)
      
      total.ask.group.D <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.04))$exchange.amount)
      
      total.ask.group.E <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.05))$exchange.amount)
      
      if(population[i,2] == .01){
        
        if((total.ask.group.A <= (population[i,3] * prop.A.to.A.give)) & (total.ask.group.A != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.B <= (population[i,3] * prop.A.to.B.give)) & (total.ask.group.B != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.C <= (population[i,3] * prop.A.to.C.give)) & (total.ask.group.C != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.D <= (population[i,3] * prop.A.to.D.give)) & (total.ask.group.D != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.E <= (population[i,3] * prop.A.to.E.give)) & (total.ask.group.E != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.A > (population[i,3] * prop.A.to.A.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
                          * (population[i,3] * prop.A.to.A.give / total.ask.group.A))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.B > (population[i,3] * prop.A.to.B.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
                          * (population[i,3] * prop.A.to.B.give / total.ask.group.B))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.C > (population[i,3] * prop.A.to.C.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
                          * (population[i,3] * prop.A.to.C.give / total.ask.group.C))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.D > (population[i,3] * prop.A.to.D.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
                          * (population[i,3] * prop.A.to.D.give / total.ask.group.D))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.E > (population[i,3] * prop.A.to.E.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
                          * (population[i,3] * prop.A.to.E.give / total.ask.group.E))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        }
        
      } else if(population[i,2] == .02){
        
        if((total.ask.group.A <= (population[i,3] * prop.B.to.A.give)) & (total.ask.group.A != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.B <= (population[i,3] * prop.B.to.B.give)) & (total.ask.group.B != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.C <= (population[i,3] * prop.B.to.C.give)) & (total.ask.group.C != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.D <= (population[i,3] * prop.B.to.D.give)) & (total.ask.group.D != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.E <= (population[i,3] * prop.B.to.E.give)) & (total.ask.group.E != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
           
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.A > (population[i,3] * prop.B.to.A.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
                          * (population[i,3] * prop.B.to.A.give / total.ask.group.A))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.B > (population[i,3] * prop.B.to.B.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
                          * (population[i,3] * prop.B.to.B.give / total.ask.group.B))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.C > (population[i,3] * prop.B.to.C.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
                          * (population[i,3] * prop.B.to.C.give / total.ask.group.C))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.D > (population[i,3] * prop.B.to.D.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
                          * (population[i,3] * prop.B.to.D.give / total.ask.group.D))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.E > (population[i,3] * prop.B.to.E.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
                          * (population[i,3] * prop.B.to.E.give / total.ask.group.E))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        }
      } else if(population[i,2] == .03){
        
        if((total.ask.group.A <= (population[i,3] * prop.C.to.A.give)) & (total.ask.group.A != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.B <= (population[i,3] * prop.C.to.B.give)) & (total.ask.group.B != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.C <= (population[i,3] * prop.C.to.C.give)) & (total.ask.group.C != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.D <= (population[i,3] * prop.C.to.D.give)) & (total.ask.group.D != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.E <= (population[i,3] * prop.C.to.E.give)) & (total.ask.group.E != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.A > (population[i,3] * prop.C.to.A.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
                          * (population[i,3] * prop.C.to.A.give / total.ask.group.A))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.B > (population[i,3] * prop.C.to.B.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
                          * (population[i,3] * prop.C.to.B.give / total.ask.group.B))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.C > (population[i,3] * prop.C.to.C.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
                          * (population[i,3] * prop.C.to.C.give / total.ask.group.C))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.D > (population[i,3] * prop.C.to.D.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
                          * (population[i,3] * prop.C.to.D.give / total.ask.group.D))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.E > (population[i,3] * prop.C.to.E.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
                          * (population[i,3] * prop.C.to.E.give / total.ask.group.E))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        }
      } else if(population[i,2] == .04){
        
        if((total.ask.group.A <= (population[i,3] * prop.D.to.A.give)) & (total.ask.group.A != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.B <= (population[i,3] * prop.D.to.B.give)) & (total.ask.group.B != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.C <= (population[i,3] * prop.D.to.C.give)) & (total.ask.group.C != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.D <= (population[i,3] * prop.D.to.D.give)) & (total.ask.group.D != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.E <= (population[i,3] * prop.D.to.E.give)) & (total.ask.group.E != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.A > (population[i,3] * prop.D.to.A.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
                          * (population[i,3] * prop.D.to.A.give / total.ask.group.A))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.B > (population[i,3] * prop.D.to.B.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
                          * (population[i,3] * prop.D.to.B.give / total.ask.group.B))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.C > (population[i,3] * prop.D.to.C.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
                          * (population[i,3] * prop.D.to.C.give / total.ask.group.C))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.D > (population[i,3] * prop.D.to.D.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
                          * (population[i,3] * prop.D.to.D.give / total.ask.group.D))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.E > (population[i,3] * prop.D.to.E.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
                          * (population[i,3] * prop.D.to.E.give / total.ask.group.E))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        }
      } else if(population[i,2] == .05){
        
        if((total.ask.group.A <= (population[i,3] * prop.E.to.A.give)) & (total.ask.group.A != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.B <= (population[i,3] * prop.E.to.B.give)) & (total.ask.group.B != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.C <= (population[i,3] * prop.E.to.C.give)) & (total.ask.group.C != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.D <= (population[i,3] * prop.E.to.D.give)) & (total.ask.group.D != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if((total.ask.group.E <= (population[i,3] * prop.E.to.E.give)) & (total.ask.group.E != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.A > (population[i,3] * prop.E.to.A.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
                          * (population[i,3] * prop.E.to.A.give / total.ask.group.A))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.B > (population[i,3] * prop.E.to.B.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
                          * (population[i,3] * prop.E.to.B.give / total.ask.group.B))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.C > (population[i,3] * prop.E.to.C.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
                          * (population[i,3] * prop.E.to.C.give / total.ask.group.C))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.D > (population[i,3] * prop.E.to.D.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
                          * (population[i,3] * prop.E.to.D.give / total.ask.group.D))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        } else if(total.ask.group.E > (population[i,3] * prop.E.to.E.give)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
                          * (population[i,3] * prop.E.to.E.give / total.ask.group.E))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
          }
        }
      }
    }
  }
  
  return(population)
}


# This function instantiates the origional matrix
initial.population.v2 <- function(){
  
  name.vector <- 1:n.population
  
  group.vector <- c(rep(0.01, n.groupA), rep(0.02, n.groupB), rep(0.03, n.groupC), rep(0.04, n.groupD), rep(0.05, n.groupE))
  
  power.vector <- c(rep(groupA.power.member, n.groupA), rep(groupB.power.member, n.groupB), rep(groupC.power.member, n.groupC),
                    rep(groupD.power.member, n.groupD), rep(groupE.power.member, n.groupE))
  
  to.A.encounter.vector <- c(rep(prob.A.to.A.encounter, n.groupA), rep(prob.B.to.A.encounter, n.groupB), rep(prob.C.to.A.encounter, n.groupC),
                             rep(prob.D.to.A.encounter, n.groupD), rep(prob.E.to.A.encounter, n.groupE))
  to.B.encounter.vector <- c(rep(prob.A.to.B.encounter, n.groupA), rep(prob.B.to.B.encounter, n.groupB), rep(prob.C.to.B.encounter, n.groupC),
                             rep(prob.D.to.B.encounter, n.groupD), rep(prob.E.to.B.encounter, n.groupE))
  to.C.encounter.vector <- c(rep(prob.A.to.C.encounter, n.groupA), rep(prob.B.to.C.encounter, n.groupB), rep(prob.C.to.C.encounter, n.groupC),
                             rep(prob.D.to.C.encounter, n.groupD), rep(prob.E.to.C.encounter, n.groupE))
  to.D.encounter.vector <- c(rep(prob.A.to.D.encounter, n.groupA), rep(prob.B.to.D.encounter, n.groupB), rep(prob.C.to.D.encounter, n.groupC),
                             rep(prob.D.to.D.encounter, n.groupD), rep(prob.E.to.D.encounter, n.groupE))
  to.E.encounter.vector <- c(rep(prob.A.to.E.encounter, n.groupA), rep(prob.B.to.E.encounter, n.groupB), rep(prob.C.to.E.encounter, n.groupC),
                             rep(prob.D.to.E.encounter, n.groupD), rep(prob.E.to.E.encounter, n.groupE))
  
  to.A.ask.vector <- c(rep(prob.A.to.A.ask, n.groupA), rep(prob.B.to.A.ask, n.groupB), rep(prob.C.to.A.ask, n.groupC),
                       rep(prob.D.to.A.ask, n.groupD), rep(prob.E.to.A.ask, n.groupE))
  to.B.ask.vector <- c(rep(prob.A.to.B.ask, n.groupA), rep(prob.B.to.B.ask, n.groupB), rep(prob.C.to.B.ask, n.groupC),
                       rep(prob.D.to.B.ask, n.groupD), rep(prob.E.to.B.ask, n.groupE))
  to.C.ask.vector <- c(rep(prob.A.to.C.ask, n.groupA), rep(prob.B.to.C.ask, n.groupB), rep(prob.C.to.C.ask, n.groupC),
                       rep(prob.D.to.C.ask, n.groupD), rep(prob.E.to.C.ask, n.groupE))
  to.D.ask.vector <- c(rep(prob.A.to.D.ask, n.groupA), rep(prob.B.to.D.ask, n.groupB), rep(prob.C.to.D.ask, n.groupC),
                       rep(prob.D.to.D.ask, n.groupD), rep(prob.E.to.D.ask, n.groupE))
  to.E.ask.vector <- c(rep(prob.A.to.E.ask, n.groupA), rep(prob.B.to.E.ask, n.groupB), rep(prob.C.to.E.ask, n.groupC),
                       rep(prob.D.to.E.ask, n.groupD), rep(prob.E.to.E.ask, n.groupE))
  
  to.A.ask.sd.vector <- c(rep(A.to.A.ask.sd, n.groupA), rep(B.to.A.ask.sd, n.groupB), rep(C.to.A.ask.sd, n.groupC),
                          rep(D.to.A.ask.sd, n.groupD), rep(E.to.A.ask.sd, n.groupE))
  to.B.ask.sd.vector <- c(rep(A.to.B.ask.sd, n.groupA), rep(B.to.B.ask.sd, n.groupB), rep(C.to.B.ask.sd, n.groupC),
                          rep(D.to.B.ask.sd, n.groupD), rep(E.to.B.ask.sd, n.groupE))
  to.C.ask.sd.vector <- c(rep(A.to.C.ask.sd, n.groupA), rep(B.to.C.ask.sd, n.groupB), rep(C.to.C.ask.sd, n.groupC),
                          rep(D.to.C.ask.sd, n.groupD), rep(E.to.C.ask.sd, n.groupE))
  to.D.ask.sd.vector <- c(rep(A.to.D.ask.sd, n.groupA), rep(B.to.D.ask.sd, n.groupB), rep(C.to.D.ask.sd, n.groupC),
                          rep(D.to.D.ask.sd, n.groupD), rep(E.to.D.ask.sd, n.groupE))
  to.E.ask.sd.vector <- c(rep(A.to.E.ask.sd, n.groupA), rep(B.to.E.ask.sd, n.groupB), rep(C.to.E.ask.sd, n.groupC),
                          rep(D.to.E.ask.sd, n.groupD), rep(E.to.E.ask.sd, n.groupE))
  
  to.A.take.sd.vector <- c(rep(A.to.A.take.sd, n.groupA), rep(B.to.A.take.sd, n.groupB), rep(C.to.A.take.sd, n.groupC),
                           rep(D.to.A.take.sd, n.groupD), rep(E.to.A.take.sd, n.groupE))
  to.B.take.sd.vector <- c(rep(A.to.B.take.sd, n.groupA), rep(B.to.B.take.sd, n.groupB), rep(C.to.B.take.sd, n.groupC),
                           rep(D.to.B.take.sd, n.groupD), rep(E.to.B.take.sd, n.groupE))
  to.C.take.sd.vector <- c(rep(A.to.C.take.sd, n.groupA), rep(B.to.C.take.sd, n.groupB), rep(C.to.C.take.sd, n.groupC),
                           rep(D.to.C.take.sd, n.groupD), rep(E.to.C.take.sd, n.groupE))
  to.D.take.sd.vector <- c(rep(A.to.D.take.sd, n.groupA), rep(B.to.D.take.sd, n.groupB), rep(C.to.D.take.sd, n.groupC),
                           rep(D.to.D.take.sd, n.groupD), rep(E.to.D.take.sd, n.groupE))
  to.E.take.sd.vector <- c(rep(A.to.E.take.sd, n.groupA), rep(B.to.E.take.sd, n.groupB), rep(C.to.E.take.sd, n.groupC),
                           rep(D.to.E.take.sd, n.groupD), rep(E.to.E.take.sd, n.groupE))
  
  prop.to.A.give.vector <- c(rep(prop.A.to.A.give, n.groupA), rep(prop.B.to.A.give, n.groupB), rep(prop.C.to.A.give, n.groupC),
                             rep(prop.D.to.A.give, n.groupD), rep(prop.E.to.A.give, n.groupE))
  prop.to.B.give.vector <- c(rep(prop.A.to.B.give, n.groupA), rep(prop.B.to.B.give, n.groupB), rep(prop.C.to.B.give, n.groupC),
                             rep(prop.D.to.B.give, n.groupD), rep(prop.E.to.B.give, n.groupE))
  prop.to.C.give.vector <- c(rep(prop.A.to.C.give, n.groupA), rep(prop.B.to.C.give, n.groupB), rep(prop.C.to.C.give, n.groupC),
                             rep(prop.D.to.C.give, n.groupD), rep(prop.E.to.C.give, n.groupE))
  prop.to.D.give.vector <- c(rep(prop.A.to.D.give, n.groupA), rep(prop.B.to.D.give, n.groupB), rep(prop.C.to.D.give, n.groupC),
                             rep(prop.D.to.D.give, n.groupD), rep(prop.E.to.D.give, n.groupE))
  prop.to.E.give.vector <- c(rep(prop.A.to.E.give, n.groupA), rep(prop.B.to.E.give, n.groupB), rep(prop.C.to.E.give, n.groupC),
                             rep(prop.D.to.E.give, n.groupD), rep(prop.E.to.E.give, n.groupE))
  
  
  return(matrix(c(name.vector, group.vector, power.vector, to.A.encounter.vector, to.B.encounter.vector, to.C.encounter.vector, to.D.encounter.vector,
                  to.E.encounter.vector, to.A.ask.vector, to.B.ask.vector, to.C.ask.vector, to.D.ask.vector, to.E.ask.vector, to.A.ask.sd.vector,
                  to.B.ask.sd.vector, to.C.ask.sd.vector, to.D.ask.sd.vector, to.E.ask.sd.vector, to.A.take.sd.vector, to.B.take.sd.vector,
                  to.C.take.sd.vector, to.D.take.sd.vector, to.E.take.sd.vector, prop.to.A.give.vector, prop.to.B.give.vector, prop.to.C.give.vector,
                  prop.to.D.give.vector, prop.to.E.give.vector), byrow=F, nrow = n.population))
}

# This function simulates one round of every person exhangeing power with every other person
exchange.power.v2 <- function(population){
  
  pop.vector <- 1:n.population
  
  index.initiated <- c()
  index.initiator <- c()
  exchange.type <- c()
  exchange.amount <- c()
  group.initiated <- c()
  group.initiator <- c()
  
  # for each person in the population...
  for(i in pop.vector){
    
    # go through each other person in the population...
    for(j in pop.vector[-i]){
      
      # check to see if person j is in group A
      if(population[j,2] == 0.01){
        
        # decide if person i will initiate contact with person j...
        # if the random number is greater than 0, the encounter is initiated
        if(runif(1, 0, 1) < population[i,4]){
          
          # a random number to choose between the three different types of encounters
          chooseEncounterType <- runif(1, 0, 1)
          
          # person i asks for power from person j
          if(chooseEncounterType <= population[i,9]){
            
            powerAsk <- abs(rnorm(1, 0, population[i,14]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            
            # person i tries to take power from person j
          } else {
            
            powerTake <- abs(rnorm(1, 0, population[i,19]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "take", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
          }
        }
        
        # check to see if person j is in group B
      } else if(population[j,2] == 0.02){
        
        # decide if person i will initiate contact with person j...
        # if the random number is greater than 0, the encounter is initiated
        if(runif(1, 0, 1) < population[i,5]){
          
          # a random number to choose between the three different types of encounters
          chooseEncounterType <- runif(1, 0, 1)
          
          # person i asks for power from person j
          if(chooseEncounterType <= population[i,10]){
            
            powerAsk <- abs(rnorm(1, 0, population[i,15]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            
            # person i tries to take power from person j
          } else {
            
            powerTake <- abs(rnorm(1, 0, population[i,20]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "take", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
          }
        }
        
        # check to see if person j is in group C
      } else if(population[j,2] == 0.03){
        # decide if person i will initiate contact with person j...
        # if the random number is greater than 0, the encounter is initiated
        if(runif(1, 0, 1) < population[i,6]){
          
          # a random number to choose between the three different types of encounters
          chooseEncounterType <- runif(1, 0, 1)
          
          # person i asks for power from person j
          if(chooseEncounterType <= population[i,11]){
            
            powerAsk <- abs(rnorm(1, 0, population[i,16]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            
            # person i tries to take power from person j
          } else {
            
            powerTake <- abs(rnorm(1, 0, population[i,21]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "take", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
          }
        }
        
        # check to see if person j is in group D
      } else if(population[j,2] == 0.04){
        # decide if person i will initiate contact with person j...
        # if the random number is greater than 0, the encounter is initiated
        if(runif(1, 0, 1) < population[i,7]){
          
          # a random number to choose between the three different types of encounters
          chooseEncounterType <- runif(1, 0, 1)
          
          # person i asks for power from person j
          if(chooseEncounterType <= population[i,12]){
            
            powerAsk <- abs(rnorm(1, 0, population[i,17]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            
            # person i tries to take power from person j
          } else {
            
            powerTake <- abs(rnorm(1, 0, population[i,22]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "take", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
          }
        }
        
        # check to see if person j is in group E
      } else if(population[j,2] == 0.05){
        # decide if person i will initiate contact with person j...
        # if the random number is greater than 0, the encounter is initiated
        if(runif(1, 0, 1) < population[i,8]){
          
          # a random number to choose between the three different types of encounters
          chooseEncounterType <- runif(1, 0, 1)
          
          # person i asks for power from person j
          if(chooseEncounterType <= population[i,13]){
            
            powerAsk <- abs(rnorm(1, 0, population[i,18]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "ask", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerAsk, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
            
            # person i tries to take power from person j
          } else {
            
            powerTake <- abs(rnorm(1, 0, population[i,23]))
            
            index.initiated <- append(index.initiated, j, after = length(index.initiated))
            index.initiator <- append(index.initiator, i, after = length(index.initiator))
            exchange.type <- append(exchange.type, "take", after = length(exchange.type))
            exchange.amount <- append(exchange.amount, powerTake, after = length(exchange.amount))
            group.initiated <- append(group.initiated, population[j, 2], after = length(group.initiated))
            group.initiator <- append(group.initiator, population[i, 2], after = length(group.initiator))
          }
        }
      }
    }
  }
  # This creates a data frame that will be used on the second and third steps of teh power exchange when people resond to people taking and asking
  # for their power. 
  return(data.frame(index.initiated, index.initiator, exchange.type, exchange.amount, group.initiated, group.initiator))
}


exchange.response.v2 <- function(intermediate.exchange.data){
  
  # response for the "takes"
  for(i in 1:n.population){
    
    total.attempted.take <- sum((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))$exchange.amount)
    
    if((0 < total.attempted.take) & (total.attempted.take < population[i,3])){
      
      # each person attempting to take power from person i gives as much power they were attempting to take to person i
      for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[,1])){
        
        power.take <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,4]
        index.loser <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,2]
        
        population[index.loser, 3] <- population[index.loser, 3] - power.take
        population[i,3] <- population[i,3] + power.take
      }
    } else if((total.attempted.take > population[i,3]) & (total.attempted.take > 0)) {
      
      for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[,1])){
        
        power.take <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,4]
        index.winner <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "take"))[j,2]
        
        population[index.winner, 3] <- population[index.winner, 3] + power.take
        
        population[i,3] <- population[i,3] - power.take
      }
    }
  }
  
  # response for the "asks"
  for(i in 1:n.population){
    
    # only give out power if your power is greater than 0!
    if(population[i,3] > 0){
      
      
      total.ask.group.A <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.01))$exchange.amount)
      
      total.ask.group.B <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.02))$exchange.amount)
      
      total.ask.group.C <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.03))$exchange.amount)
      
      total.ask.group.D <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.04))$exchange.amount)
      
      total.ask.group.E <- sum((intermediate.exchange.data %>% 
                                  filter(index.initiated == i) %>% 
                                  filter(exchange.type == "ask") %>% 
                                  filter(group.initiator == 0.05))$exchange.amount)
      
        
        if((total.ask.group.A <= (population[i,3] * population[i,24])) & (total.ask.group.A != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.A > (population[i,3] * population[i,24])){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,4]
                          * (population[i,3] * population[i,24] / total.ask.group.A))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.01))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.B <= (population[i,3] * population[i,25])) & (total.ask.group.B != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.B > (population[i,3] * population[i,25])){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,4]
                          * (population[i,3] * population[i,25] / total.ask.group.B))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.02))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.C <= (population[i,3] * population[i,26])) & (total.ask.group.C != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.C > (population[i,3] * population[i,26])){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,4]
                          * (population[i,3] * population[i,26] / total.ask.group.C))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.03))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.D <= (population[i,3] * population[i,27])) & (total.ask.group.D != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.D > (population[i,3] * population[i,27])){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,4]
                          * (population[i,3] * population[i,27] / total.ask.group.D))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.04))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if((total.ask.group.E <= (population[i,3] * population[i,28])) & (total.ask.group.E != 0)){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        } else if(total.ask.group.E > (population[i,3] * population[i,28])){
          
          for(j in 1:length((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[,1])){
            
            power.ask <- ((intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,4]
                          * (population[i,3] * population[i,28] / total.ask.group.E))
            index.asker <- (intermediate.exchange.data %>% filter(index.initiated == i) %>% filter(exchange.type == "ask") %>% filter(group.initiator == 0.05))[j,2]
            
            population[index.asker, 3] <- population[index.asker, 3] + power.ask
            population[i,3] <- population[i,3] - power.ask
            
          }
        }
      }
  }
  return(population)
}

# this selection process takes all of the members of each group who are more than one standard deviation below the group average power level and replaces them with the stats of the most 
# powerful person in their group, keeping the same power level. 
genetic.algorithm <- function(population){
  
  group.A.most.powerful.index <- 0
  group.B.most.powerful.index <- 0
  group.C.most.powerful.index <- 0
  group.D.most.powerful.index <- 0
  group.E.most.powerful.index <- 0
  
  group.A.total.power <- 0
  group.B.total.power <- 0
  group.C.total.power <- 0
  group.D.total.power <- 0
  group.E.total.power <- 0
  
  for(i in 1:n.population){
    if(population[i,2] == 0.01){
      group.A.total.power <- group.A.total.power + population[i,3]
      
      if(group.A.most.powerful.index == 0){
        group.A.most.powerful.index <- i
      } else if(population[i,3] > population[group.A.most.powerful.index,3]){
        group.A.most.powerful.index <- i
      }
    } else if(population[i,2] == 0.02){
      group.B.total.power <- group.B.total.power + population[i,3]
      
      if(group.B.most.powerful.index == 0){
        group.B.most.powerful.index <- i
      } else if(population[i,3] > population[group.B.most.powerful.index,3]){
        group.B.most.powerful.index <- i
      }
    } else if(population[i,2] == 0.03){
      group.C.total.power <- group.C.total.power + population[i,3]
      
      if(group.C.most.powerful.index == 0){
        group.C.most.powerful.index <- i
      } else if(population[i,3] > population[group.C.most.powerful.index,3]){
        group.C.most.powerful.index <- i
      }
    } else if(population[i,2] == 0.04){
      group.D.total.power <- group.D.total.power + population[i,3]
      
      if(group.D.most.powerful.index == 0){
        group.D.most.powerful.index <- i
      } else if(population[i,3] > population[group.D.most.powerful.index,3]){
        group.D.most.powerful.index <- i
      }
    } else{
      group.E.total.power <- group.E.total.power + population[i,3]
      
      if(group.E.most.powerful.index == 0){
        group.E.most.powerful.index <- i
      } else if(population[i,3] > population[group.E.most.powerful.index,3]){
        group.E.most.powerful.index <- i
      }
    }
  }
  
  group.A.average.power <- group.A.total.power / n.groupA
  group.B.average.power <- group.B.total.power / n.groupB
  group.C.average.power <- group.C.total.power / n.groupC
  group.D.average.power <- group.D.total.power / n.groupD
  group.E.average.power <- group.E.total.power / n.groupE
  
  # this for loop takes all of the members of each group who are more than group.*.sd.selection below the group average power level and replaces them with the stats of the most 
  # powerful person in their group, keeping the same power level.
  for(i in 1:n.population){
    
    if(population[i,2] == 0.01){
      
      if(population[i,3] < group.A.average.power - group.A.sd.selection) {
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[group.A.most.powerful.index,4], population[group.A.most.powerful.index,5],
                            population[group.A.most.powerful.index,6], population[group.A.most.powerful.index,7], population[group.A.most.powerful.index,8],
                            population[group.A.most.powerful.index,9], population[group.A.most.powerful.index,10], population[group.A.most.powerful.index,11],
                            population[group.A.most.powerful.index,12], population[group.A.most.powerful.index,13], population[group.A.most.powerful.index,14],
                            population[group.A.most.powerful.index,15], population[group.A.most.powerful.index,16], population[group.A.most.powerful.index,17],
                            population[group.A.most.powerful.index,18], population[group.A.most.powerful.index,19], population[group.A.most.powerful.index,20],
                            population[group.A.most.powerful.index,21], population[group.A.most.powerful.index,22], population[group.A.most.powerful.index,23],
                            population[group.A.most.powerful.index,24], population[group.A.most.powerful.index,25], population[group.A.most.powerful.index,26],
                            population[group.A.most.powerful.index,27], population[group.A.most.powerful.index,18])
      }
    } else if(population[i,2] == 0.02){
      
      if(population[i,3] < group.B.average.power - group.B.sd.selection) {
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[group.B.most.powerful.index,4], population[group.B.most.powerful.index,5],
                            population[group.B.most.powerful.index,6], population[group.B.most.powerful.index,7], population[group.B.most.powerful.index,8],
                            population[group.B.most.powerful.index,9], population[group.B.most.powerful.index,10], population[group.B.most.powerful.index,11],
                            population[group.B.most.powerful.index,12], population[group.B.most.powerful.index,13], population[group.B.most.powerful.index,14],
                            population[group.B.most.powerful.index,15], population[group.B.most.powerful.index,16], population[group.B.most.powerful.index,17],
                            population[group.B.most.powerful.index,18], population[group.B.most.powerful.index,19], population[group.B.most.powerful.index,20],
                            population[group.B.most.powerful.index,21], population[group.B.most.powerful.index,22], population[group.B.most.powerful.index,23],
                            population[group.B.most.powerful.index,24], population[group.B.most.powerful.index,25], population[group.B.most.powerful.index,26],
                            population[group.B.most.powerful.index,27], population[group.B.most.powerful.index,18])
      }
    } else if(population[i,2] == 0.03){
      
      if(population[i,3] < group.C.average.power - group.C.sd.selection) {
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[group.C.most.powerful.index,4], population[group.C.most.powerful.index,5],
                            population[group.C.most.powerful.index,6], population[group.C.most.powerful.index,7], population[group.C.most.powerful.index,8],
                            population[group.C.most.powerful.index,9], population[group.C.most.powerful.index,10], population[group.C.most.powerful.index,11],
                            population[group.C.most.powerful.index,12], population[group.C.most.powerful.index,13], population[group.C.most.powerful.index,14],
                            population[group.C.most.powerful.index,15], population[group.C.most.powerful.index,16], population[group.C.most.powerful.index,17],
                            population[group.C.most.powerful.index,18], population[group.C.most.powerful.index,19], population[group.C.most.powerful.index,20],
                            population[group.C.most.powerful.index,21], population[group.C.most.powerful.index,22], population[group.C.most.powerful.index,23],
                            population[group.C.most.powerful.index,24], population[group.C.most.powerful.index,25], population[group.C.most.powerful.index,26],
                            population[group.C.most.powerful.index,27], population[group.C.most.powerful.index,18])
      }
    } else if(population[i,2] == 0.04){
      
      if(population[i,3] < group.D.average.power - group.D.sd.selection) {
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[group.D.most.powerful.index,4], population[group.D.most.powerful.index,5],
                            population[group.D.most.powerful.index,6], population[group.D.most.powerful.index,7], population[group.D.most.powerful.index,8],
                            population[group.D.most.powerful.index,9], population[group.D.most.powerful.index,10], population[group.D.most.powerful.index,11],
                            population[group.D.most.powerful.index,12], population[group.D.most.powerful.index,13], population[group.D.most.powerful.index,14],
                            population[group.D.most.powerful.index,15], population[group.D.most.powerful.index,16], population[group.D.most.powerful.index,17],
                            population[group.D.most.powerful.index,18], population[group.D.most.powerful.index,19], population[group.D.most.powerful.index,20],
                            population[group.D.most.powerful.index,21], population[group.D.most.powerful.index,22], population[group.D.most.powerful.index,23],
                            population[group.D.most.powerful.index,24], population[group.D.most.powerful.index,25], population[group.D.most.powerful.index,26],
                            population[group.D.most.powerful.index,27], population[group.D.most.powerful.index,18])
      }
    } else if(population[i,2] == 0.05){
      
      if(population[i,3] < group.E.average.power - group.E.sd.selection) {
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[group.E.most.powerful.index,4], population[group.E.most.powerful.index,5],
                            population[group.E.most.powerful.index,6], population[group.E.most.powerful.index,7], population[group.E.most.powerful.index,8],
                            population[group.E.most.powerful.index,9], population[group.E.most.powerful.index,10], population[group.E.most.powerful.index,11],
                            population[group.E.most.powerful.index,12], population[group.E.most.powerful.index,13], population[group.E.most.powerful.index,14],
                            population[group.E.most.powerful.index,15], population[group.E.most.powerful.index,16], population[group.E.most.powerful.index,17],
                            population[group.E.most.powerful.index,18], population[group.E.most.powerful.index,19], population[group.E.most.powerful.index,20],
                            population[group.E.most.powerful.index,21], population[group.E.most.powerful.index,22], population[group.E.most.powerful.index,23],
                            population[group.E.most.powerful.index,24], population[group.E.most.powerful.index,25], population[group.E.most.powerful.index,26],
                            population[group.E.most.powerful.index,27], population[group.E.most.powerful.index,18])
      }
    }
  }
  
  # this for loop takes all of the members of each group who are less than the group average power level plus the group.*.sd.mutation and mutates their behavior slightly
  for(i in 1:n.population){
    
    
    if(population[i,2] == 0.01){
      
      if(population[i,3] < group.A.average.power + group.A.sd.mutation){
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[i,4] + rnorm(1, 0, mutation.sd), population[i,5] + rnorm(1, 0, mutation.sd), population[i,6] + rnorm(1, 0, mutation.sd),
                            population[i,7] + rnorm(1, 0, mutation.sd), population[i,8] + rnorm(1, 0, mutation.sd), population[i,9] + rnorm(1, 0, mutation.sd), population[i,10] + rnorm(1, 0, mutation.sd),
                            population[i,11] + rnorm(1, 0, mutation.sd), population[i,12] + rnorm(1, 0, mutation.sd), population[i,13] + rnorm(1, 0, mutation.sd), population[i,14] + rnorm(1, 0, mutation.sd),
                            population[i,15] + rnorm(1, 0, mutation.sd), population[i,16] + rnorm(1, 0, mutation.sd), population[i,17] + rnorm(1, 0, mutation.sd), population[i,18] + rnorm(1, 0, mutation.sd), 
                            population[i,19] + rnorm(1, 0, mutation.sd), population[i,20] + rnorm(1, 0, mutation.sd), population[i,21] + rnorm(1, 0, mutation.sd), population[i,22] + rnorm(1, 0, mutation.sd),
                            population[i,23] + rnorm(1, 0, mutation.sd), population[i,24] + rnorm(1, 0, mutation.sd), population[i,25] + rnorm(1, 0, mutation.sd), population[i,26] + rnorm(1, 0, mutation.sd),
                            population[i,27] + rnorm(1, 0, mutation.sd), population[i,28] + rnorm(1, 0, mutation.sd))
      }
    } else if(population[i,2] == 0.02){
      
      if(population[i,3] < group.B.average.power + group.B.sd.mutation){
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[i,4] + rnorm(1, 0, mutation.sd), population[i,5] + rnorm(1, 0, mutation.sd), population[i,6] + rnorm(1, 0, mutation.sd),
                            population[i,7] + rnorm(1, 0, mutation.sd), population[i,8] + rnorm(1, 0, mutation.sd), population[i,9] + rnorm(1, 0, mutation.sd), population[i,10] + rnorm(1, 0, mutation.sd),
                            population[i,11] + rnorm(1, 0, mutation.sd), population[i,12] + rnorm(1, 0, mutation.sd), population[i,13] + rnorm(1, 0, mutation.sd), population[i,14] + rnorm(1, 0, mutation.sd),
                            population[i,15] + rnorm(1, 0, mutation.sd), population[i,16] + rnorm(1, 0, mutation.sd), population[i,17] + rnorm(1, 0, mutation.sd), population[i,18] + rnorm(1, 0, mutation.sd), 
                            population[i,19] + rnorm(1, 0, mutation.sd), population[i,20] + rnorm(1, 0, mutation.sd), population[i,21] + rnorm(1, 0, mutation.sd), population[i,22] + rnorm(1, 0, mutation.sd),
                            population[i,23] + rnorm(1, 0, mutation.sd), population[i,24] + rnorm(1, 0, mutation.sd), population[i,25] + rnorm(1, 0, mutation.sd), population[i,26] + rnorm(1, 0, mutation.sd),
                            population[i,27] + rnorm(1, 0, mutation.sd), population[i,28] + rnorm(1, 0, mutation.sd))
      }
    } else if(population[i,2] == 0.03){
      
      if(population[i,3] < group.C.average.power + group.C.sd.mutation){
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[i,4] + rnorm(1, 0, mutation.sd), population[i,5] + rnorm(1, 0, mutation.sd), population[i,6] + rnorm(1, 0, mutation.sd),
                            population[i,7] + rnorm(1, 0, mutation.sd), population[i,8] + rnorm(1, 0, mutation.sd), population[i,9] + rnorm(1, 0, mutation.sd), population[i,10] + rnorm(1, 0, mutation.sd),
                            population[i,11] + rnorm(1, 0, mutation.sd), population[i,12] + rnorm(1, 0, mutation.sd), population[i,13] + rnorm(1, 0, mutation.sd), population[i,14] + rnorm(1, 0, mutation.sd),
                            population[i,15] + rnorm(1, 0, mutation.sd), population[i,16] + rnorm(1, 0, mutation.sd), population[i,17] + rnorm(1, 0, mutation.sd), population[i,18] + rnorm(1, 0, mutation.sd), 
                            population[i,19] + rnorm(1, 0, mutation.sd), population[i,20] + rnorm(1, 0, mutation.sd), population[i,21] + rnorm(1, 0, mutation.sd), population[i,22] + rnorm(1, 0, mutation.sd),
                            population[i,23] + rnorm(1, 0, mutation.sd), population[i,24] + rnorm(1, 0, mutation.sd), population[i,25] + rnorm(1, 0, mutation.sd), population[i,26] + rnorm(1, 0, mutation.sd),
                            population[i,27] + rnorm(1, 0, mutation.sd), population[i,28] + rnorm(1, 0, mutation.sd))
      }
    } else if(population[i,2] == 0.04){
      
      if(population[i,3] < group.D.average.power + group.D.sd.mutation){
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[i,4] + rnorm(1, 0, mutation.sd), population[i,5] + rnorm(1, 0, mutation.sd), population[i,6] + rnorm(1, 0, mutation.sd),
                            population[i,7] + rnorm(1, 0, mutation.sd), population[i,8] + rnorm(1, 0, mutation.sd), population[i,9] + rnorm(1, 0, mutation.sd), population[i,10] + rnorm(1, 0, mutation.sd),
                            population[i,11] + rnorm(1, 0, mutation.sd), population[i,12] + rnorm(1, 0, mutation.sd), population[i,13] + rnorm(1, 0, mutation.sd), population[i,14] + rnorm(1, 0, mutation.sd),
                            population[i,15] + rnorm(1, 0, mutation.sd), population[i,16] + rnorm(1, 0, mutation.sd), population[i,17] + rnorm(1, 0, mutation.sd), population[i,18] + rnorm(1, 0, mutation.sd), 
                            population[i,19] + rnorm(1, 0, mutation.sd), population[i,20] + rnorm(1, 0, mutation.sd), population[i,21] + rnorm(1, 0, mutation.sd), population[i,22] + rnorm(1, 0, mutation.sd),
                            population[i,23] + rnorm(1, 0, mutation.sd), population[i,24] + rnorm(1, 0, mutation.sd), population[i,25] + rnorm(1, 0, mutation.sd), population[i,26] + rnorm(1, 0, mutation.sd),
                            population[i,27] + rnorm(1, 0, mutation.sd), population[i,28] + rnorm(1, 0, mutation.sd))
      }
    } else if(population[i,2] == 0.05){
      
      if(population[i,3] < group.E.average.power + group.E.sd.mutation){
        population[i,] <- c(population[i,1], population[i,2], population[i,3], population[i,4] + rnorm(1, 0, mutation.sd), population[i,5] + rnorm(1, 0, mutation.sd), population[i,6] + rnorm(1, 0, mutation.sd),
                            population[i,7] + rnorm(1, 0, mutation.sd), population[i,8] + rnorm(1, 0, mutation.sd), population[i,9] + rnorm(1, 0, mutation.sd), population[i,10] + rnorm(1, 0, mutation.sd),
                            population[i,11] + rnorm(1, 0, mutation.sd), population[i,12] + rnorm(1, 0, mutation.sd), population[i,13] + rnorm(1, 0, mutation.sd), population[i,14] + rnorm(1, 0, mutation.sd),
                            population[i,15] + rnorm(1, 0, mutation.sd), population[i,16] + rnorm(1, 0, mutation.sd), population[i,17] + rnorm(1, 0, mutation.sd), population[i,18] + rnorm(1, 0, mutation.sd), 
                            population[i,19] + rnorm(1, 0, mutation.sd), population[i,20] + rnorm(1, 0, mutation.sd), population[i,21] + rnorm(1, 0, mutation.sd), population[i,22] + rnorm(1, 0, mutation.sd),
                            population[i,23] + rnorm(1, 0, mutation.sd), population[i,24] + rnorm(1, 0, mutation.sd), population[i,25] + rnorm(1, 0, mutation.sd), population[i,26] + rnorm(1, 0, mutation.sd),
                            population[i,27] + rnorm(1, 0, mutation.sd), population[i,28] + rnorm(1, 0, mutation.sd))
      }
    }
  }
  
  return(population)
}


# this function runs the power exchange, selection, and mutation process n number of times
run.n.gens <- function(n, population){
  
  for(i in 1:n){
    intermediate.exchange.data <- exchange.power.v2(population)
    population <- exchange.response.v2(intermediate.exchange.data)
    population <- genetic.algorithm(population)
  }
  
  return(population)
}

population <- initial.population.v2()
population <- run.n.gens(1, population)
population

intermediate.exchange.data <- exchange.power.v2(population)
population <- exchange.response.v2(intermediate.exchange.data)
population <- genetic.algorithm(population)
