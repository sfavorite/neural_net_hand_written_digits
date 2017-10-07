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

print("Checking Cost Function (w/ Regularization) using lambda 1...")
print("Cost should be about 0.383770")
lambda <- 1

J <- nnCostFunction(nn_params, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
print(sprintf("Cost is : %f", J[[1]]))
cat("\n")
print("Evaluating sigmoid gradient at [1, -0.5, 0, 0.5, 1]")
print("Values should be:   0.196612 0.235004 0.250000 0.235004 0.196612")
known_values <- c(0.196612, 0.235004, 0.250000, 0.235004, 0.196612)
g = sigmoidGradient(c(1, -0.5, 0, 0.5, 1))
print(sprintf("G: %f", g))
print(sprintf("They match? %s", identical(round(g, 6) , known_values)))
cat("\n")

# Create random weights to match the input/hidden/output(num_labels)
print("Initializing Neural Network Parameters")
cat("\n")
initial_Theta1 = randomInitialWeights(input_layer_size, hidden_layer_size)
initial_Theta2 = randomInitialWeights(hidden_layer_size, num_labels)

# Unroll parameters
t1 <- unlist(initial_Theta1)
t2 <- unlist(initial_Theta2)

# Setup initial network parameters to use in optima() below
initial_nn_params <- as.vector(c(t1, t2))

# Check to make sure backpropagation is working
print("Checking Backpropagation")
checkNNGradients()

lambda <- 3
print('Checking Backpropagation (w/ lambda = 3)')
checkNNGradients(lambda)

debug_J <- nnCost(initial_nn_params, input_layer_size, hidden_layer_size, num_labels, x, y, lambda)
debug_J[[1]]

print("Begin training Neural Net")
cat("\n")

# Pre-specify the procedures named parameters and use the return as the new procedure
# We will use this in the optim() function
lambda <- 2
costF <- Curry(nnCost, input_layer_size=input_layer_size, hidden_layer_size=hidden_layer_size, num_labels=num_labels, x=x, y=y, lambda=lambda)
grad <- Curry(nnGrad, input_layer_size=input_layer_size, hidden_layer_size=hidden_layer_size, num_labels=num_labels, x=x, y=y, lambda=lambda)

# There are lots of different options for optim
# Some different configs are commented out...please see ?optim() for more information 
ctrl <- list(maxit=100, type=1)
#ctrl <- list(maxit=100)
#theta_optim <- optim(par=initial_nn_params, fn=costF, method="CG",  gr=grad, control = ctrl)
theta_optim <- optim(par=initial_nn_params, fn=costF, method="BFGS",  gr=grad, control = ctrl)
#theta_optim <- optim(par=initialTheta, fn=costF, x=x, lambda=lambda, y=this_y, gr=grad, method="BFGS", control = list(maxit=50))

# Obtain Theta1 and Theta2 back from nn_params
nn_params <- theta_optim$par
Theta1 <- matrix(nn_params[1:(hidden_layer_size * ( input_layer_size + 1))], nrow=hidden_layer_size, ncol=input_layer_size+1)
Theta2 <- matrix(nn_params[1+(hidden_layer_size * ( input_layer_size + 1)):(length(nn_params)-1)], nrow=num_labels, ncol=hidden_layer_size+1)

pred <- nnPredict(Theta1, Theta2, x)

print(sprintf("Training Set accuracy: %f", mean(pred==y) * 100))
