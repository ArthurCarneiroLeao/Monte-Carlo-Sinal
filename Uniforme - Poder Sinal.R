#####################################################
#-------Programa Poder do Teste de Sinal-------######

#UNIFORME#
library(BSDA)
library(tidyverse)


r<-1000
theta.test<-c(seq(-2,2,0.05))
M <- length(theta.test)
power <- numeric(M)
nobs<-c(5, 10, 30, 80)                 #Vetor Para tamanhos de amostras diferentes
power_nobs <- matrix(0,length(nobs),M) #criando o ambiente(matriz) para armazenamento
cont <- 1
for (j in nobs){
  for (i in 1:M) {
    theta<-theta.test[i]
    p_value <- replicate(r, expr = {
      x <- runif(j, min = theta - sqrt(3), max = theta + sqrt(3))
      SinalTest<-SIGN.test(x)
      SinalTest$p.value })
    power[i] <- mean(p_value <= 0.05)
  }
  power_nobs[cont,] <- power
  cont = cont+1
}

#Gráfico para n diferentes.
x11()
par(mfrow=c(2,2))
plot(theta.test, power_nobs[1,], type = "l", xlab = bquote(theta), ylab = "Poder", main = "n = 5")
abline(v = 0.0, lwd = 2, col = "grey80", lty = 2)

plot(theta.test, power_nobs[2,], type = "l", xlab = bquote(theta), ylab = "Poder", main = "n = 10")
abline(v = 0.0, lwd = 2, col = "grey80", lty = 2)

plot(theta.test, power_nobs[3,], type = "l", xlab = bquote(theta), ylab = "Poder", main = "n = 30")
abline(v = 0.0, lwd = 2, col = "grey80", lty = 2)

plot(theta.test, power_nobs[4,], type = "l", xlab = bquote(theta), ylab = "Poder", main = "n = 80")
abline(v = 0.0, lwd = 2, col = "grey80", lty = 2)
