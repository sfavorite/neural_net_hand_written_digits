# Make a small neural network and dataset and check the gradients 
checkNNGradients <- function(lambda = 0) {
      input_layer_size <- 3
      hidden_layer_size <- 5
      num_labels <- 3
      m <- 5
      
      # Create weights for input (Theta1) and hidden (Theta2) layers 
      Theta1 <- debugInitializeWeights(hidden_layer_size, input_layer_size)
      Theta2 <- debugInitializeWeights(num_labels, hidden_layer_size)
      
      # Make some dummy data 
      x <- debugInitializeWeights(m, input_layer_size - 1)
      y <- 1 + 1:m %% num_labels
      
      # Unroll parameters
      t1 <- unlist(Theta1)
      t2 <- unlist(Theta2)
      # Parameters for use in costFunc 
      nn_params <- as.vector(c(t1, t2))
      
      # Setup a "partially determined" procedure to send to function computeNumericalGradient 
      costFunc <- Curry(nnCostFunction, input_layer_size=input_layer_size, hidden_layer_size=hidden_layer_size, num_labels=num_labels, x=x, y=y, lambda=lambda);
      
      # Compute the analytical gradient 
      result <- costFunc(nn_params)
      print(sprintf("Analytical Gradient: %f", sum(result[[2]])))
      #print(sum(result[[2]]))
      
      # Compute the numericalgradient 
      numgrad <- computeNumericalGradient(costFunc, nn_params)      
      print(sprintf("Numerical Gradient: %f", sum(numgrad)))
      print("The two outputs should be close in values")
      #print(sum(numgrad))
      
      # Compute the relative difference 
      diff <- norm(numgrad-result[[2]])/norm(numgrad+result[[2]])
      print("If backpropogation is correct the relative difference will be small (less that 1e-9)")
      print(sprintf("Relative Difference: %f", diff))
      cat("\n")
      
}
