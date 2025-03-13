#Solucion Pregunta 1
x <- c(0,1)
fx <- c(0.68, 0.32)

#tabla de probabilidad
cbind(x, fx)

plot(x, fx, pch=16, col="red", ylim = c(0,1))
lines(x, fx,type="h", col="red")

mu <- sum(x*fx)
mu
sigmasq <- sum((x-mu)^2*fx)
sigmasq

sample(x,1)
sample(x,2, prob = fx)
#si no posas el replace lasbolas que treus njo es tornen a introduir
sample(x,2, prob = fx, replace= TRUE)
n<- 43
sample(x,n, prob = fx, replace= TRUE)

sum(sample(x,n, prob = fx, replace= TRUE))

y<- function(i){sum(sample(x,n, prob = fx, replace= TRUE))}
y(10)

#bucle
m<- 400000
muestra <- sapply(1:m, y)
fi<- table(muestra)/m
fi


#frecuencias relativas
data.frame(fi, fi=cumsum(fi))

barplot(fi)

###
dbinom(13, 43, 0.32)

#tabla de proba
data.frame(y=0.43,Prob=dbinom(0:43,43, 0.32))

Y<- 0:43
fy <- f
  
dbinom(13, 43, 0.32)


y<- 0:44
pi<- dbinom(y, 44, 0.32)
df<- data.frame(Y=y,Prob=dbinom(0:44,44, 0.32))
Fi<- cumsum(df$Prob)

cbind(0:44, dbinom(0:44, 44, 0.32), Fi)


