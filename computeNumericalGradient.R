# Computes the gradient using "finite differences" to numerically approximate the gradient

computeNumericalGradient <- function(J, theta) {
      
      numgrad = matrix(0, nrow=length(theta), ncol=1)
      perturb = matrix(0, nrow=length(theta), ncol=1)
      
      e = 1e-4;
      end <- length(theta)
      for (p in 1:end) {
            perturb[p] <- e;
            loss1 <- J(theta - perturb)[[1]];
            loss2 <- J(theta + perturb)[[1]];
            # Compute Numerical Gradient
            numgrad[p] <- (loss2 - loss1) / (2*e);
            perturb[p] <- 0;
      }
      return(numgrad)
}