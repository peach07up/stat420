# Example 1a
x <- c(15.5, 16.2, 16.1, 15.8, 15.6, 16.0, 15.8, 15.9, 16.2)
mean(x)
sum(x)/length(x)

sd(x)
sqrt(var(x))


# Example 1b
# All at once
t.test(x, alternative = c("two.sided"), conf.level = 0.95)

tt <- t.test(x, alternative = c("two.sided"), conf.level = 0.95)
tt$conf.int

# Piece by piece
qt(0.975, 8)
mean(x) - qt(0.975, 8) * sd(x) / sqrt(9)
mean(x) + qt(0.975, 8) * sd(x) / sqrt(9)


# Example 1c
t.test(x, mu = 16, alternative = c("less"), conf.level = 0.95)

  

# Example 2
x <- c(70,82,78,74,94,82)
y <- c(64,72,60,76,72,80,84,68)
# All at once
t.test(x, y, alternative = c("greater"), var.equal=TRUE)

# Piece by piece
Spooled2 <- ((6 - 1) * var(x) + (8 - 1) * var(y)) / (6 + 8 - 2)
Spooled2

test_stat <- (mean(x) - mean(y)) / sqrt(Spooled2 * (1 / 6 + 1 / 8))
test_stat
1 - pt(test_stat, 6 + 8 - 2)

  

# Example 3a
z <- 1 / sqrt(0.32)
z
pnorm(z) - pnorm(-z)

  
# Example 3b
N <- 25
mu1 <- 6
mu2 <- 5
std <- 2

S <- 1000
count <- 0
diffall <- c(1:S)

for (i in 1:S){
  x1 <- rnorm(N, mu1, std)
  x2 <- rnorm(N, mu2, std)
  diffall[i] <- mean(x1) - mean(x2)
  if ((diffall[i] > 0) & (diffall[i] < 2)){
    count <- count + 1}}

count / S

hist(diffall)

qqnorm(diffall)
qqline(diffall)

