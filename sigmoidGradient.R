sigmoidGradient <- function(z) {
      # Create a matrix of the values 
      g <- matrix(0, nrow(z), nrow(z))
      
      g <- sigmoid(z) * (1 - sigmoid(z))
      return (g)
}