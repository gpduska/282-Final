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


n.population <- 100000  #size of population

# These change how the measurement of an identity is skewed. For example if most of the population is white, there wouldn't be an even distribution
# of the race as an identity. 
i1Measure.shift <- f(x) = x 
i2Measure.shift <- f(x) = x 
i3Measure.shift <- f(x) = x 
i4Measure.shift <- f(x) = x 

# This is a measure of how power is distributed proportionally relating to different identities
# This gets multiplied by a person's iTarget vectors.
i1Weight <- 0.25
i2Weight <- 0.25
i3Weight <- 0.25
i4Weight <- 0.25


# This function instantiates the origional matrix
initial.population <- function(n.population){
  
  name.vector <- 1:n.population
  
  powerScore.vector <- rep(1, n.population)
  
  i1Score.vector <- rep(i1Weight, n.population)
  i2Score.vector <- rep(i2Weight, n.population)
  i3Score.vector <- rep(i3Weight, n.population)
  i4Score.vector <- rep(i4Weight, n.population)
  
  powerRank.vector <- c(1:n.population)
  
  i1Rank.vector <- c(1:n.population)
  i2Rank.vector <- c(1:n.population)
  i3Rank.vector <- c(1:n.population)
  i4Rank.vector <- c(1:n.population)
  
  i1Target.vector <- rep(NA, n.population)
  i2Target.vector <- rep(NA, n.population)
  i3Target.vector <- rep(NA, n.population)
  i4Target.vector <- rep(NA, n.population)
  
  i1Measure.vector <- sample(1:n.population, n.population, replace = FALSE)
  i2Measure.vector <- sample(1:n.population, n.population, replace = FALSE)
  i3Measure.vector <- sample(1:n.population, n.population, replace = FALSE)
  i4Measure.vector <- sample(1:n.population, n.population, replace = FALSE)
  
  return(matrix(c(name.vector, powerScore.vector, i1Score.vector, i2Score.vector, i2Score.vector, i4Score.vector, powerRank.vector,
                  i1Rank.vector, i2Rank.vector, i3Rank.vector, i4Rank.vector, i1Target.vector, i2Target.vector, i3Target.vector, i4Target.vector,
                  i1Measure.vector, i2Measure.vector, i3Measure.vector, i4Measure.vector), byrow=F, nrow = n.population))
}

# This function takes the initial population matrix which does not yet have the iTarget columns filled in and gives each person an iTarget that makes
# sense according to thier i#Measure perameters
init.iTargets <- (pop.matrix){
  
  leftGreedy1 <- function(pop.matrix)
  
}



# Every person should have a powerScore goal. The greedy goal would be to maximize your own power. My idea is to have a neural net for each person that 
# is figuring out how to distribute power in such a way that brings them closer to their goal. 


