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

Person(name, group, powerScore, prob.of.A.encounter, prob.ask.from.A, prob.give.to.A, prob., prob.of.B.encounter, prob.of.C.encounter,)

n.population <- 3000  #size of population

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
prob.A.to.A.encounter <- 0.1
prob.A.to.B.encounter <- 0.1
prob.A.to.C.encounter <- 0.1
prob.A.to.D.encounter <- 0.1
prob.A.to.E.encounter <- 0.1

prob.B.to.A.encounter <- 0.1
prob.B.to.B.encounter <- 0.1
prob.B.to.C.encounter <- 0.1
prob.B.to.D.encounter <- 0.1
prob.B.to.E.encounter <- 0.1

prob.C.to.A.encounter <- 0.1
prob.C.to.B.encounter <- 0.1
prob.C.to.C.encounter <- 0.1
prob.C.to.D.encounter <- 0.1
prob.C.to.E.encounter <- 0.1

prob.D.to.A.encounter <- 0.1
prob.D.to.B.encounter <- 0.1
prob.D.to.C.encounter <- 0.1
prob.D.to.D.encounter <- 0.1
prob.D.to.E.encounter <- 0.1

prob.E.to.A.encounter <- 0.1
prob.E.to.B.encounter <- 0.1
prob.E.to.C.encounter <- 0.1
prob.E.to.D.encounter <- 0.1
prob.E.to.E.encounter <- 0.1

# the number that a random number between 0 and 1 has to be less than in order for an ask of power to happen
prob.A.to.A.ask <- 0
prob.A.to.B.ask <- 0
prob.A.to.C.ask <- 0
prob.A.to.D.ask <- 0
prob.A.to.E.ask <- 0

prob.B.to.A.ask <- 0
prob.B.to.B.ask <- 0
prob.B.to.C.ask <- 0
prob.B.to.D.ask <- 0
prob.B.to.E.ask <- 0

prob.C.to.A.ask <- 0
prob.C.to.B.ask <- 0
prob.C.to.C.ask <- 0
prob.C.to.D.ask <- 0
prob.C.to.E.ask <- 0

prob.D.to.A.ask <- 0
prob.D.to.B.ask <- 0
prob.D.to.C.ask <- 0
prob.D.to.D.ask <- 0
prob.D.to.E.ask <- 0

prob.E.to.A.ask <- 0
prob.E.to.B.ask <- 0
prob.E.to.C.ask <- 0
prob.E.to.D.ask <- 0
prob.E.to.E.ask <- 0

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

# This function instantiates the origional matrix
initial.population <- function(){
  
  name.vector <- 1:n.population
  
  group.vector <- c(rep(0.1, n.groupA), rep(0.2, n.groupB), rep(0.3, n.groupC), rep(0.4, n.groupD), rep(0.5, n.groupE))
  
  power.vector <- c(rep(groupA.power.member, n.groupA), rep(groupB.power.member, n.groupB), rep(groupC.power.member, n.groupC),
                    rep(groupD.power.member, n.groupD), rep(groupE.power.member, n.groupE))
  
  return(matrix(c(name.vector, group.vector, power.vector), byrow=F, nrow = n.population))
}
population <- initial.population()
population


# This function simulates one round of every person exhangeing power with every other person
exchange.power <- function(population){
  
  intermediate.ask.matrix <- matrix(rep(list(c(NA)), n.population * 5), byrow=F, nrow = n.population)
  intermediate.take.matrix <- matrix(rep(list(c(NA)), n.population), byrow=F, nrow = n.population)
  
  # for each person in the population...
  for(i in 1:n.population){
    
    # check to see if person i is in group A
    if(population[i, 2] == 0.01){
      
      # go through each other person in the population...
      for(j in 1:npopulation[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.A.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.A.to.A.ask){
              
              powerAsk <- rnorm(1, 0, A.to.A.ask.sd)
              
              intermediate.ask.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, powerAsk), after = length(intermediate.take.matrix[j, 1]))
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.A.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
            
            
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.B.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.C.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, A.to.D.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, A.to.E.take.mean, A.to.E.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
            }
          }
        }
      }
      
      #check to see if person i is in group B
    } else if(population[i, 2] == 0.02){
      
      # go through each other person in the population...
      for(j in 1:npopulation[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.B.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.B.to.A.ask){
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.A.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.B.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.C.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.D.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, B.to.E.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
            }
          }
        }
      }
      
      #check to see if person i is in group C
    } else if(population[i, 2] == 0.03){
      
      # go through each other person in the population...
      for(j in 1:npopulation[-i]){
        
        # check to see if person is j in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.C.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.C.to.A.ask){
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.A.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.B.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.C.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.D.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, C.to.E.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
            }
          }
        }
      }
      
      #check to see if person i is in group D
    } else if(population[i, 2] == 0.04){
      
      # go through each other person in the population...
      for(j in 1:npopulation[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.D.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.D.to.A.ask){
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.A.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
             
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.B.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.C.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.D.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, D.to.E.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
            }
          }
        }
      }
      
      #check to see if person i is in group E
    } else if(population[i, 2] == 0.05){
      
      # go through each other person in the population...
      for(j in 1:npopulation[-i]){
        
        # check to see if person j is in group A
        if(population[j,2] == 0.01){
          
          # decide if person i will initiate contact with person j...
          # if the random number is greater than 0, the encounter is initiated
          if(runif(1, 0, 1) < prob.E.to.A.encounter){
            
            # a random number to choose between the three different types of encounters
            chooseEncounterType <- runif(1, 0, 1)
            
            # person i asks for power from person j
            if(chooseEncounterType <= prob.E.to.A.ask){
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.A.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.B.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.C.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.D.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
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
              
              
              # person i tries to take power from person j
            } else {
              
              powerTake <- abs(rnorm(1, 0, E.to.E.take.sd))
              
              intermediate.take.matrix[j, 1] <- 
                append(intermediate.take.matrix[j, 1], list(i, population[i, 3], powerTake), after = length(intermediate.take.matrix[j, 1]))
            }
          }
        }
      }
    }
  }
}



