################################################################################
# Mathematical Calculations
################################################################################

# Addition and Subtraction
3 + 2
3 - 2

# Multiplication and Division
3 * 2
3 / 2

# Exponents in R
3^2
2^(-3)
100^(1/2)
sqrt(1/2)
exp(1)

# Constants in R
pi
exp(1)

# Logarithms
log(10)        # natural log
log10(1000)    # base 10 log
log2(8)        # base 2 log
log(16,base=4) # base 4 log

# Trigonometry
sin(pi/2)
cos(0)

# Help with R
?log
?sin

################################################################################
# Vectors and Matrices
################################################################################

# Creating Vectors
x <- c(1,3,5,7,8,9)
x
y <- 1:20
y
z <- seq(1,2,0.1)
z

# Returning Elements of a Vector
x[3]
x[1:3]
x[-2]

# Vectorization
x <- 1:10
x + 1
2 * x
2 ^ x
sqrt(x)
log(x)

# Matrices
x <- 1:9
x
X <- matrix(x,nrow=3,ncol=3)
X
Y <- matrix(x,nrow=3,ncol=3,byrow=TRUE)
Y
Z <- matrix(0,2,4)
Z
X
X[1,2]
X[1,]
X[,2]
X[2,c(1,3)]
rev(x)
rep(1,9)
cbind(x,rev(x),rep(1,9))
rbind(x,rev(x),rep(1,9))

# Some Linear Algebra
x <- 1:9
y <- 9:1
X <- matrix(x,3,3)
Y <- matrix(y,3,3)
X
Y
X + Y
X - Y
X * Y
X / Y
X %*% Y
t(X)
#solve(X) # ERROR
Z <- matrix(c(9,2,-3,2,4,-2,-3,-2,16),3,byrow=T)
Z
solve(Z)
X <- matrix(1:6,2,3)
X
dim(X)
rowSums(X)
colSums(X)
rowMeans(X)
colMeans(X)
diag(Z)
diag(1:5)
diag(5)


################################################################################
# Distributions
################################################################################

dnorm(3, mean = 2, sd = 5)
pnorm(3, mean = 2, sd = 5)
qnorm(0.975, mean = 2, sd = 5)
rnorm(10, mean = 2, sd = 5)



################################################################################
# Basic Programming
################################################################################

# Logical Operators
heights <- c(110, 120, 115, 136, 205, 156, 175)
weights <- c(64, 67, 62, 60, 77, 70, 66)
heights < 121 | heights == 156
weights[heights > 150]

# If/Else
x <- 10
y <- 0
if (x >5) {
  x <- x / 2
  y <- 2 * x
} else {
  x <- x * 2
  y <- x
}
x
y

# For loops
x <- 11:15
for(i in 1:5){
  x[i] <- x[i]+1
}
x

# Functions
standardize <- function(x) {
  m <- mean(x)
  std <- sd(x)
  result <- (x - m) / std
  result
}
x <- rnorm(10,2,25)
standardize(x)
