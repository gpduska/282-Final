# 282-Final

# The RadicalizationModel.R file is the origional model where I was attempting to use group identity as a perameter that helped determine how 
# agents interact with eachother. I was never able to get this model up and running.

# The RadicalizationModelSimple.R is the second model that I made. This model does ot use group identity. In this model, every agent has two 
# perameters for each other agent that determine the liklihood that the agent will attempt to take power and the amount of power that they will 
# attempt to take. In the model, agents accumulated power between rounds which was detrimental to the gentic algorithm. Because of this, I made a third and final model.

# The RadMod3.R file is the final model that I made. It is very similar to the model in RadicalizationModelSimple.R. The main differences are the 
# in the new model, agents do not accumulate power between rounds (instead they are given a new random power score at the beginning of each round),
# and I rewrote the exchange.power function so that I did not have to use a data frame. This made the model much faster. 