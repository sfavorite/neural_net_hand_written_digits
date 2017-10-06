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

# Load the data 
load("ex4data.RData")

# Convert the image (x) and labels (y) to a matrix 
x <- as.matrix(data[,1:400])
y <- as.matrix(data[,401])

# How many samples do we have
m <- nrow(x)
