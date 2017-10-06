# Set weights using "sin" so that debugging...if needed...is easier. 
debugInitializeWeights <- function(fan_out, fan_in) {
      
      # Create a matrix of all zeros
      #W <- matrix(0, nrow = fan_out, ncol = fan_in + 1)
      
      #size <- nrow(W) * ncol(W)
      #t <- matrix(sin(1:size), nrow=nrow(W), ncol=ncol(W)) / 10
      
      # All of the above shortend to...
      
      
      # Get the size for the matrix 
      size <- fan_out * (fan_in + 1)
      # Set the values using "sin" so that W is always the same which 
      # can be useful in debugging. 
      t <- matrix(sin(1:size), nrow=fan_out, ncol=(fan_in + 1)) / 10
      
}
