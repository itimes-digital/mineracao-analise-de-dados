install.packages('FNN')
install.packages('caret')
library(class)
library(caret)
library(FNN)

load("D:/estudo-machine-learning/mineracao-analise-de-dados/trilha 02/nonlin.Rdata")

par(mfrow=c(1,5))
x = xtrain[,1]
y = ytrain[,1]
plot(x, y)
lines(x0, r0, lwd=2)

linmodel = lm(y~x)
plot(x, y)
abline(a=linmodel$coef[1], b=linmodel$coef[2], col='red', lwd=2)

ks = c(3,15,45)
knnmodel1 = knn.reg(train=matrix(x, ncol = 1), 
                    test=matrix(x0, ncol = 1), 
                    y=y,k=ks[1])

knnmodel2 = knn.reg(train=matrix(x, ncol = 1), 
                    test=matrix(x0, ncol = 1), 
                    y=y,k=ks[2])

knnmodel3 = knn.reg(train=matrix(x, ncol = 1), 
                    test=matrix(x0, ncol = 1), 
                    y=y,k=ks[3])

#par(mfrow=c(1,3))
plot(x, y, main=paste("k = ", ks[1]))
lines(x0, knnmodel1$pred, col = "red", lwd=2)
plot(x, y, main=paste("k = ", ks[2]))
lines(x0, knnmodel2$pred, col = "green", lwd=2)
plot(x, y, main=paste("k = ", ks[3]))
lines(x0, knnmodel3$pred, col = "red", lwd=2)

