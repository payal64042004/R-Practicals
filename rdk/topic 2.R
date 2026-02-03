m=c(4,3,2,1)
s=matrix(c(3,0,2,2,
           0,1,1,0,
           2,1,9,-2,
           2,0,-2,4),nrow=4,byrow=T)
p=4
n=150
q=2
x=c(3,2)

m1=m[1:q]
m2=m[(q+1):p]
s11=s[1:q,1:q]
s12=s[1:q,(q+1):p]
s21=t(s12)
s22=s[(q+1):p,(q+1):p]
s22_inv=solve(s22)
cm=m1+s12%*%s22_inv%*%(x-m2)
cs=s[1:q,1:q]-s12%*%s22_inv%*%s21
library(MASS)
cx=t(mvrnorm(n,cm,cs))
cm_hat=rowMeans(cx)
cs_hat=cov(t(cx))
cr=cov2cor(cs)
cr_hat=cor(t(cx))
cat("conditional mean (cm): \n")
print(cm)
cat("estimated conditional mean (cm_hat): \n")
print(cm_hat)
cat("conditional covariance matrix(cs): \n")
print(cs)
cat("estimated conditional covariance matrix(cs_hat): \n")
print(cs_hat)
cat("correlation matrix(cr):\n")
print(cr)
cat("estmated correlation matrix(cr_hat):\n")
print(cr_hat)


