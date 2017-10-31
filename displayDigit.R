load("ex4data.RData")
x <- as.matrix(data[,1:400])

# Display one digit 
displayDigit <- function(X){
      m <- matrix(unlist(X),nrow = 20,byrow = T)
      m <- t(apply(m, 2, rev))
      image(m,col=grey.colors(255))
}

# Display the first digit
displayDigit(x[1, ])



# Display 12 random digits 
par(mfrow=c(4,5))

# Using Ing's data 
#for(i in seq(from=1, to=5000, by=500)) {
for (i in 1:20) {
      #row <- i
      random_row <- sample(1:5000, 1)
      X <- x[random_row, ]
      m <- matrix(unlist(X),nrow = 20,byrow = T)
      M <- t(apply(m, 2, rev))
      
      #image(m, col = grey.colors(255))
      # Re from 20 to one to reverse the digit
      
      image(m[, 20:1])
}

par(mfrow=c(2,5))

# Using MNIST data (currently set to MNIST)
for(i in seq(from=1, to=5000, by=500)){
      print(i)
      row <- i
      #random_row <- sample(1:5000, 1)
      X <- x[row, ]
      m <- matrix(unlist(X),nrow = 20,byrow = T)
      M <- t(apply(m, 2, rev))
      #image(m, col = grey.colors(255))
      image(m[,20:1])
      #text(0.5, 85, "par(mfrow)", cex=2)
}


par(mfrow=c(5,5))
par(mar=c(0,0,0,0))

for(i in 1:25){m = matrix(readBin(the_file, integer(), size=1, n=28*28, endian="big"),28,28);image(m[,28:1])}
