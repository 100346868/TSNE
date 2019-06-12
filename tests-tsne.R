###############################################################################
#THIS IS TSNE IMPLEMENTED BY ALFONSO ALBACETE
#FOR HIS FINAL DEGREE PROJECT
###############################################################################

#In this script there are some test to try the algorithm

#Iris
library(Rtsne)
data <- as.matrix(unique(iris[,-5]))
Test <- tsne(data,perplexity = 35, learning_rate = 100, momentum = 0.5, T= 1e4, 
             cols = iris$Species)
Test$Y_0
Test$Y_t

plot(Test, col = iris$Species,pch = 15)
RtSNE = Rtsne(data, perplexity = 30, eta = 10)$Y
plot(RtSNE, col = iris$Species, pch = 15)



#aver <- tsne(x,perplexity = 17,learning_rate = 10, momentum = 0.5, T= 1e4, 
#             cols = p)

#Mixture of gaussians
n = 150
n = 200
data <- rbind(mvtnorm::rmvnorm(n = n, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = n, mean = c(3, 3)))
plot(data, pch = 15, col = rep(viridis::viridis(2), each = 150))
Test <- tsne(data, perplexity = 50, learning_rate = 200, momentum = 0.5,
               cols = rep(viridis::viridis(2), each = 150), T = 1e4)


#iris in 3d

data <- as.matrix(unique(iris[,-5]))
OurTsne <- tsne(data,perplexity = 35, learning_rate = 200,q = 3, momentum = 0.5, 
             T= 1e3, cols = iris$Species)
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
scatterplot3d(OurTsne, pch = 16, color=colors)

RtSNE = Rtsne(data, dims = 3, eta = 200, perplexity = 30)
color = iris$Species
colors = colors[as.numeric(color)]

scatterplot3d(RtSNE$Y, pch = 16,color = colors)


# Daring to something more ambicious
# Glass dataset: https://www.kaggle.com/uciml/glass 

glass <- read.csv("C:/Users/Alfonso/Desktop/glass.csv")
data <- as.matrix(unique(glass))
Test <- tsne(data,perplexity = 35, learning_rate = 200,q = 2, momentum = 0.5, 
             T= 4e3, cols = glass$Type)
#plot(Test, pch = 15, col= glass$Type)


##Now Im trying the same procedure with the original Function

##Diferent eta plots
library(Rtsne)
par(mfrow=c(1,3))
test <-  Rtsne(data, perplexity = 30, learning_rate = 10)
plot(test$Y, pch = 15, col = iris$Species)
test2 <-  Rtsne(data, perplexity = 30, learning_rate = 100)
plot(test2$Y, pch = 15, col = iris$Species)
test3 <-  Rtsne(data, perplexity = 30, learning_rate = 1000)
plot(test3$Y, pch = 15, col = iris$Species)

##Diferent perp plots
library(Rtsne)
par(mfrow=c(1,6))
plot(data, pch = 15, col = rep(viridis::viridis(2), each = 200))
perp5 <-  tsne(data, perplexity = 5, learning_rate = 100)
plot(perp5, pch = 15, col = rep(viridis::viridis(2), each = 200))
perp10 <-  tsne(data, perplexity = 10, learning_rate = 100)
plot(perp10, pch = 15, col = rep(viridis::viridis(2), each = 200))
perp30 <-  tsne(data, perplexity = 30, learning_rate = 100)
plot(perp30, pch = 15, col = rep(viridis::viridis(2), each = 200))
perp50 <-  tsne(data, perplexity = 50, learning_rate = 100)
plot(perp50, pch = 15, col = rep(viridis::viridis(2), each = 200))
perp100 <-  tsne(data, perplexity = 400, learning_rate = 100)
plot(perp100, pch = 15, col = rep(viridis::viridis(2), each = 200))





##For mixture of gaussians

library(mvtnorm)
data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)))
plot(data, pch = 15, col = rep(viridis::viridis(2), each = 150) ) 

test <-  Rtsne(data, perplexity = 50 )

plot(test$Y, pch = 15,col = rep(viridis::viridis(2), each = 150) )




data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)),
              mvtnorm::rmvnorm(n = 150, mean = c(10, 10)))
plot(data, pch = 15, col = rep(viridis::viridis(3), each = 150))
Test <- tsne(data, perplexity = 50, learning_rate = 200, momentum = 0.5,
             cols = rep(viridis::viridis(3), each = 150), T = 1e4)
plot(test$Y, pch = 15,col = rep(viridis::viridis(3), each = 150) )


##With the glass data

glass <- read.csv("C:/Users/Alfonso/Desktop/glass.csv")
data <- as.matrix(unique(glass))
pr <- Rtsne(data,perplexity = 35,momentum = 0.5)
plot(pr$Y, pch = 15, col = glass$Type)


##Subplots in R

test <-  Rtsne(as.matrix(unique(iris[,-5])), perplexity = 35, eta = 10)
par(mfrow=c(3,1))
plot(test$Y, pch = 15, col = iris$Species)
test <-  Rtsne(as.matrix(unique(iris[,-5])), perplexity = 35, eta = 100)
plot(test$Y, pch = 15, col = iris$Species)
test <-  Rtsne(as.matrix(unique(iris[,-5])), perplexity = 35, eta = 1000)
plot(test$Y, pch = 15, col = iris$Species)



#TAKES > 20 min
mnist <- read.csv("C:/Users/Alfonso/Desktop/mnist.csv")
#quitar priemra columna
#etiquetas primera columna
#predecir las labels en funcion de los datos y viceversa
#

data <- as.matrix(unique(mnist))
pr <- Rtsne(data,dims = 2,perplexity = 100,verbose=TRUE,momentum = 0.5)
plot(pr$Y, pch = 17, col = mnist$label)

#Prueba mnist con RtSNE

data <- as.matrix(unique(mnist))
mnist2 = mnist[,-1]
pr <- Rtsne(mnist2,dims = 3,perplexity = 100,verbose=TRUE,momentum = 0.5)
plot(pr$Y, pch = 17, col = mnist$label)

     

#### CUADRADO DE APLIACCION A DIFERENTES GAUSIANAS####

#dos gaussianas mismo tamaño separadas
par(mfrow=c(4,3))
data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)))
plot(data, pch = 15, col = rep(viridis::viridis(2), each = 150))

ourTsne <- tsne(data, perplexity = 35, learning_rate = 200, momentum = 0.5,
             cols = rep(viridis::viridis(2), each = 150), T = 1e3)

plot(ourTsne,col = rep(viridis::viridis(2), each = 150), pch = 15)
RtSNE <-  Rtsne(data, perplexity = 35, eta = 200 )
plot(RtSNE$Y, pch = 15,col = rep(viridis::viridis(2), each = 150) )
## tres gaussianas, la del medio mayor sigma mismo tamaño

data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19)), 
              mvtnorm::rmvnorm(n = 150, mean = c(0, 0),
              sigma = rbind(c(10, 0.2), c(0.2, 10))),
              mvtnorm::rmvnorm(n = 150, mean = c(19,19)))
#data
plot(data2, pch = 15, col = rep(viridis::viridis(3), each = 150))
ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
               , T = 1e3)
plot(ourTsne,col = rep(viridis::viridis(3), each = 150), pch = 15)
RtSNE <-  Rtsne(data2, perplexity = 35, eta = 200 )

plot(RtSNE$Y, pch = 15,col = rep(viridis::viridis(3), each = 150) )

## 4 gaussianas diferentes tamaños
#Gaussianas diferentes tamaños
set.seed(123546)
k = 4
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.4,0.2,0.3,0.1))

data3 <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(15,-15)),
              mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(12,-12)),
              mvtnorm::rmvnorm(n = length(s[s==3]), mean = c(-5,-5)),
              mvtnorm::rmvnorm(n = length(s[s==4]), mean = c(10,10)))


plot(data3, col = sort(s), pch = 15)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne,col = sort(s), pch = 15)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200 )

plot(RtSNE$Y, pch = 15,col = sort(s) )

### 4 gaussianas con diferentes tamaños y variaciones

set.seed(123546)
k = 4
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.4,0.2,0.3,0.1))

data3 <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(15,-15)),
               mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(12,-12)),
               mvtnorm::rmvnorm(n = length(s[s==3]), mean = c(-5,-5),
                                sigma = rbind(c(10, 0.2), c(0.2, 10)) ),
               mvtnorm::rmvnorm(n = length(s[s==4]), mean = c(10,10)))


plot(data3, col = sort(s), pch = 15)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne,col = sort(s), pch = 15)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200 )

plot(RtSNE$Y, pch = 15,col = sort(s) )







#Gaussianas diferentes tamaños
set.seed(123546)
k = 2
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.3,0.7))

data <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(1,1)),
              mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(-1,-1)))

#Por que salen diferentes resultados
plot(data, col = sort(s))

plot(Rtsne(data)$Y, col = sort(s))

#Gaussianas diferentes tamaños
set.seed(123546)
k = 3
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.3,0.5,0.2))

data <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(5,5)),
              mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(-10,-10)),
              mvtnorm::rmvnorm(n = length(s[s==3]), mean = c(0,0)))

#Por que salen diferentes resultados
plot(data, col = sort(s))

plot(Rtsne(data, perplexity = 30, eta = 200)$Y, col = sort(s))

## UNa gaussiana con mayor sigma variacion

data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19)), 
               mvtnorm::rmvnorm(n = 150, mean = c(0, 0), 
                                sigma = rbind(c(10, 0.2), c(0.2, 10))),
               mvtnorm::rmvnorm(n = 150, mean = c(19,19)))
plot(data2)

########################################################???
### GAUSSIANAS EN 3D

sigma = rbind(c(10, 0.2,0.2), c(0.2, 10,0.2),c(0.2, 0.2,10))
n= 300
data2 <- rbind(mvtnorm::rmvnorm(n = n, mean = c(-19, -19,4)), 
               mvtnorm::rmvnorm(n = n, mean = c(0, 0,-20)),
               mvtnorm::rmvnorm(n = n, mean = c(19,19,20)))

par(mfrow=c(1,3))
plot(data2, col = rep(viridis::viridis(3), each = n))

s <- sample(1:3,n,replace = TRUE, prob = c(0.33,0.33,0.33))
scatterplot3d(data2, color  = rep(viridis::viridis(3), each = 300))

RtSNE <- Rtsne(data2,dims = 3)

scatterplot3d(RtSNE$Y, color =  rep(viridis::viridis(3), each = 300))

ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 3)

scatterplot3d(ourTsne, color =  rep(viridis::viridis(3), each = 300))

### GAUSSIANAS 4D


n= 400
data4 <- rbind(mvtnorm::rmvnorm(n = n, mean = c(-19, -19,4,20)), 
               mvtnorm::rmvnorm(n = n, mean = c(0, 0,-20,-15)),
               mvtnorm::rmvnorm(n = n, mean = c(19,19,20,0)))

par(mfrow=c(1,2))
pairs(data4, col =  rep(viridis::viridis(3), each = 400))



RtSNE <- Rtsne(data4,dims = 3)

scatterplot3d(RtSNE$Y, color =  rep(viridis::viridis(3), each = 400))

ourTsne <- tsne(data4, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 3)

scatterplot3d(ourTsne, color = rep(viridis::viridis(3), each = 400))


#### Segundo intento de Cuadrado diferentes gaussianas


#### CUADRADO DE APLIACCION A DIFERENTES GAUSIANAS####

#dos gaussianas mismo tamaño separadas
par(mfrow=c(4,3))
data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)))

plot(data, pch = 15, col = rep(viridis::viridis(2), each = 150))

ourTsne <- tsne(data, perplexity = 35, learning_rate = 200, momentum = 0.5,
                cols = rep(viridis::viridis(2), each = 150), T = 1e3)

plot(ourTsne$Y_t,col = rep(viridis::viridis(2), each = 150), pch = 15)

RtSNE <-  Rtsne(data, perplexity = 35, eta = 200, Y_init = ourTsne$Y_0 )
plot(RtSNE$Y, pch = 15,col = rep(viridis::viridis(2), each = 150) )
## tres gaussianas, la del medio mayor sigma mismo tamaño

data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19)), 
               mvtnorm::rmvnorm(n = 150, mean = c(0, 0),
                                sigma = rbind(c(10, 0.2), c(0.2, 10))),
               mvtnorm::rmvnorm(n = 150, mean = c(19,19)))
#data
plot(data2, pch = 15, col = rep(viridis::viridis(3), each = 150))
ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = rep(viridis::viridis(3), each = 150), pch = 15)
RtSNE <-  Rtsne(data2, perplexity = 35, eta = 200,Y_init =ourTsne$Y_0  )

plot(RtSNE$Y, pch = 15,col = rep(viridis::viridis(3), each = 150) )

## 4 gaussianas diferentes tamaños
#Gaussianas diferentes tamaños
set.seed(123546)
k = 4
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.4,0.2,0.3,0.1))

data3 <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(15,-15)),
               mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(12,-12)),
               mvtnorm::rmvnorm(n = length(s[s==3]), mean = c(-5,-5)),
               mvtnorm::rmvnorm(n = length(s[s==4]), mean = c(10,10)))


plot(data3, col = sort(s), pch = 15)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = sort(s), pch = 15)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200,Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 15,col = sort(s) )

### 4 gaussianas con diferentes tamaños y variaciones

set.seed(123546)
k = 4
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.4,0.2,0.3,0.1))

data3 <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(15,-15)),
               mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(12,-12)),
               mvtnorm::rmvnorm(n = length(s[s==3]), mean = c(-5,-5),
                                sigma = rbind(c(10, 0.2), c(0.2, 10)) ),
               mvtnorm::rmvnorm(n = length(s[s==4]), mean = c(10,10)))


plot(data3, col = sort(s), pch = 15)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = sort(s), pch = 15)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200, Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 15,col = sort(s) )


# SI da error de figure margins par(mar=c(1,1,1,1))







###### FIGURAS EXOTICAS
par(mfrow=c(1,3))

n= 150
data4 <- rbind(mvtnorm::rmvnorm(n = n, mean = c(6,6)
              ,sigma = rbind(c(6,0.8),c(0.8,6) )  ), 
               mvtnorm::rmvnorm(n = n, mean = c(0,6)
                                
                                 ),
               mvtnorm::rmvnorm(n = n, mean = c(3,3)
                                
              ),
              
               mvtnorm::rmvnorm(n = n, mean = c(0,0)
                                ,sigma = rbind(c(6,0.8),c(0.8,6) )
              ),
               mvtnorm::rmvnorm(n = n, mean = c(6,0)
                                 
              
              ))
#sigma = rbind(c(0.1,0.1))
#sigma = rbind(c(0.1,0.1, 0.1,0.1), c(0.1,0.1,0.1,0.1), c(0.1,0.1,0.1,0.1), c(0.1,0.1,0.1,0.1))
plot(data4,pch = 15, col =  rep(viridis::viridis(5), each = 150))
ourTsne <- tsne(data4, perplexity = 70, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 2)
plot(ourTsne$Y_t, pch =15, col =  rep(viridis::viridis(5), each = n))
RtSNE <-  Rtsne(data4, perplexity = 70, eta = 200, Y_init = ourTsne$Y_0)
plot(RtSNE$Y, pch = 15,col = rep(viridis::viridis(5), each = 150))


##EXOTIC 2

n= 150
data4 <- rbind(mvtnorm::rmvnorm(n = n, mean = c(10,10)
                                ,sigma = rbind(c(0.8,0.8),c(0.8,0.8) )  ), 
               mvtnorm::rmvnorm(n = n, mean = c(6,6)
                                ,sigma = rbind(c(0.8,0.8),c(0.8,0.8) ) 
               ),
               mvtnorm::rmvnorm(n = n, mean = c(2,2)
                                ,sigma = rbind(c(0.8,0.8),c(0.8,0.8) ) 
                                
               ),
               
               mvtnorm::rmvnorm(n = n, mean = c(1,1)
                                ,sigma = rbind(c(0.8,0.8),c(0.8,0.8) )
               ),
               mvtnorm::rmvnorm(n = n, mean = c(0,0)
                                ,sigma = rbind(c(0.8,0.8),c(0.8,0.8) ) 
                                
                                
               ))

plot(data4,pch = 15, col =  rep(viridis::viridis(5), each = 150))
ourTsne <- tsne(data4, perplexity = 70, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 2)
plot(ourTsne$Y_t, pch =15, col =  rep(viridis::viridis(5), each = n))
RtSNE <-  Rtsne(data4, perplexity = 70, eta = 200, Y_init = ourTsne$Y_0)
plot(RtSNE$Y, pch = 15,col = rep(viridis::viridis(5), each = 150))





################### Segundo intento gaussianas 3d 3 dimensiones
### GAUSSIANAS EN 3D
#par(mar=c(1,1,1,1))
par(mfrow=c(4,3))
par(mfrow=c(1,3))
#mismo tamaño
sigma = rbind(c(10, 0.2,0.2), c(0.2, 10,0.2),c(0.2, 0.2,10))
n= 300
data2 <- rbind(mvtnorm::rmvnorm(n = n, mean = c(-19, -19,4)), 
               mvtnorm::rmvnorm(n = n, mean = c(0, 0,-20)),
               mvtnorm::rmvnorm(n = n, mean = c(19,19,20)))

#par(mfrow=c(1,3))
#plot(data2, col = rep(viridis::viridis(3), each = n))
#s <- sample(1:3,n,replace = TRUE, prob = c(0.33,0.33,0.33))
scatterplot3d(data2, color  = rep(viridis::viridis(3), each = 300))
ourTsne <- tsne(data2, perplexity = 50, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 2)
plot(ourTsne$Y_t, pch =15, col =  rep(viridis::viridis(3), each = n))
RtSNE <- Rtsne(data2,dims = 2, perplexity = 50, Y_init = ourTsne$Y_0)
plot(RtSNE$Y, pch = 15, col =  rep(viridis::viridis(3), each = n))


## 3 gaussianas una distinta dispersion


data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19,-19)), 
               mvtnorm::rmvnorm(n = 150, mean = c(0, 0,0),
          sigma = rbind(c(1, 0.2,0.2), c(0.2, 1,0.2), c(0.2, 0.2, 1))),
               mvtnorm::rmvnorm(n = 150, mean = c(19,19,19)))
#data
scatterplot3d(data2, pch = 15, color = rep(viridis::viridis(3), each = 150))

ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = rep(viridis::viridis(3), each = 150), pch = 15)

RtSNE <-  Rtsne(data2, perplexity = 35, eta = 200,Y_init = ourTsne$Y_0  )

plot(RtSNE$Y, pch = 15,col = rep(viridis::viridis(3), each = 150) )


## 4 gaussianas diferentes tamaños
#Gaussianas diferentes tamaños
set.seed(123546)
k = 4
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.4,0.2,0.3,0.1))

data3 <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(15,-15,15)),
               mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(12,-12,12)),
               mvtnorm::rmvnorm(n = length(s[s==3]), mean = c(-5,-5,-5)),
               mvtnorm::rmvnorm(n = length(s[s==4]), mean = c(10,10,10)))


scatterplot3d(data3, color = sort(s), pch = 15)

ourTsne <- tsne(data3, perplexity = 50, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = sort(s), pch = 15)
RtSNE <-  Rtsne(data3, perplexity = 50, eta = 200,Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 15,col = sort(s) )

### Gausianas distintos tamaños y variaciones

set.seed(123546)
k = 4
n = 200

s <- sample(1:k,n,replace = TRUE, prob = c(0.4,0.2,0.3,0.1))

data4 <- rbind(mvtnorm::rmvnorm(n = length(s[s==1]), mean = c(15,-15,15)),
               mvtnorm::rmvnorm(n = length(s[s==2]), mean = c(12,-12,12)),
               mvtnorm::rmvnorm(n = length(s[s==3]), mean = c(-5,-5,-5),
              sigma = rbind(c(1, 0.2,0.2), c(0.2, 1,0.2), c(0.2,0.2,1)) ),
               mvtnorm::rmvnorm(n = length(s[s==4]), mean = c(10,10,10)))


scatterplot3d(data4, color = sort(s), pch = 15)


ourTsne <- tsne(data4, perplexity = 50, learning_rate = 200, momentum = 0.5
                , T = 1e3)

plot(ourTsne$Y_t,col = sort(s), pch = 15)
RtSNE <-  Rtsne(data4, perplexity = 50, eta = 200, Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 15,col = sort(s) )

#Iris segundo intento con y init
library(Rtsne)
data <- as.matrix(unique(iris[,-5]))
Test <- tsne(data,perplexity = 40, learning_rate = 200, momentum = 0.5, T= 2e3, 
             cols = iris$Species)


plot(Test$Y_t, col = iris$Species,pch = 15)
RtSNE <- Rtsne(data, perplexity = 40, eta = 200, Y_init = Test$Y_0)
plot(RtSNE$Y, col = iris$Species, pch = 15)





#iris in 3d

data <- as.matrix(unique(iris[,-5]))
OurTsne <- tsne(data,perplexity = 35, learning_rate = 200,q = 3, momentum = 0.5, 
                T= 1e3, cols = iris$Species)
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
z<- as.matrix(colors)
scatterplot3d(OurTsne$Y_t, pch = 16, color=z[-150,])


RtSNE = Rtsne(data, dims = 3, eta = 200, perplexity = 30, Y_init = OurTsne$Y_0 )
color = iris$Species
colors = colors[as.numeric(color)]

scatterplot3d(RtSNE$Y, pch = 16,color = z[-150,])
