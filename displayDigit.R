# Display one digit 
displayDigit <- function(X){
      m <- matrix(unlist(X),nrow = 20,byrow = T)
      m <- t(apply(m, 2, rev))
      image(m,col=grey.colors(255))
}

# Display the first digit
displayDigit(x[1, ])



# Display 12 random digits 
par(mfrow=c(3,4))

for (i in 1:12) {
      
      random_row <- sample(1:5000, 1)
      X <- x[random_row, ]
      m <- matrix(unlist(X),nrow = 20,byrow = T)
      M <- t(apply(m, 2, rev))
      image(m, col = grey.colors(255))
}
