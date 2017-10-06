# Neural Network using back propagation for hand-written digit recognition - this is based on code from Andrew Ing's ML course

# functional will provide our optimization function: optim()  
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

# Initial weights provided by Andrew Ing...only needed for testing functions.
Theta1 <- read.csv("theta1.csv", header=FALSE)
Theta2 <- read.csv("theta2.csv", header=FALSE)

# Unroll parameters
t1 <- unlist(Theta1)
t2 <- unlist(Theta2)
nn_params <- as.vector(c(t1, t2))

print("Feedforward Using Neural Network ...")
cat("\n")
lambda <- 0
J <- nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
print("Cost should be 0.287629")
print(sprintf("Cost is: %f", J[[1]]))
cat("\n")

