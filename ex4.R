# Neural Network using back propagation for hand-written digit recognition - this is based on code from Andrew Ing's ML course

library('functional')
source("nnCostFunction.R")
source("logistic_functions.R")
source("randInitialWeights.R")
source("debugInitializeWeights.R")
source("checkNNGradients.R")
source("computeNumericalGradient.R")
source("nnCost.R")
source("nnGrad.R")
source("predict.R")

# Set initial paramaters
input_layer_size <- 400
hidden_layer_size <- 25
num_labels <- 10

load("ex4data.RData")
