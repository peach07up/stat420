Y = c(56, 23, 52, 28, 35, 43, 25, 16, 27, 32, 47, 43, 52, 61, 74, 16, 14, 18, 27, 31, 58, 62, 68, 72, 83, 15, 14, 22, 16, 27)
A = c( 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)
B = c( 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
model = glm(Y ~ factor(A) * factor(B))
summary(aov(model))

interaction.plot(A,B,Y)
interaction.plot(B,A,Y)


# Cell means
means = matrix(NA,2,3)
for(a in 1:2)
{
  for(b in 1:3)
  {
    means[a,b] = mean(Y[A==a & B==b])
  }
}
means


# for 2 plots on the same page
par(mfrow=c(1,2))

plot(Y~A,col=B,pch=B)
for(b in 1:3)
 {
  lines(1:2,means[,b],col=b)
 }

plot(Y~B,col=A,pch=A)
for(a in 1:2)
 {
  lines(1:3,means[a,],col=a)
 }
