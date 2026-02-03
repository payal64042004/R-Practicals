library(matrixcalc)
library(MASS)
library(Matrix)
mu=(c(4.32,14.01,1.95,2.17,2.45))
s=matrix(c(4.308,1.683,1.803,2.155,-0.253,
           1.683,1.768,0.588,0.177,0.176,
           1.803,0.588,0.81,1.065,-0.158,
           2.155,0.177,1.065,1.970,-0.357,
           -0.253,0.176,-0.158,-0.357,0.504),nrow=5,byrow=TRUE)
p=5
n=200
c=t(chol(s))
c
t(c)%*%c
x=matrix(NA,p,n)
for (i in 1:n){
  y=as.matrix(rnorm(p,0,1))
  x[,i]=y
}
X_corr= mu+(c%*%x)
(x_final=t(X_corr))
mu_hat=rowMeans(x)
mu_hat
colMeans(x_final)
cbind(mu,mu_hat)

z=diag(c(diag(s)))
sqrt(z)
R=cov2cor(s)



# example 5
library(MASS)
library(Matrix)
mu=(c(4.32,14.01,1.95,2.17,2.45))
s=matrix(c(4.308,1.683,1.803,2.155,-0.253,
           1.683,1.768,0.588,0.177,0.176,
           1.803,0.588,0.81,1.065,-0.158,
           2.155,0.177,1.065,1.970,-0.357,
           -0.253,0.176,-0.158,-0.357,0.504),nrow=5,byrow=TRUE)
p=5
n=200
x=mvrnorm(n,mu,s)
t(x)
mu_hat=cov(t(x))
s_hat=cov(t(x))
cbind(s,s_hat)
