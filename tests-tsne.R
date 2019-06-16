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

plot(Test, col = iris$Species,pch = 16)
RtSNE = Rtsne(data, perplexity = 30, eta = 10)$Y
plot(RtSNE, col = iris$Species, pch = 16)



#aver <- tsne(x,perplexity = 17,learning_rate = 10, momentum = 0.5, T= 1e4, 
#             cols = p)

#Mixture of gaussians
n = 150
n = 200
data <- rbind(mvtnorm::rmvnorm(n = n, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = n, mean = c(3, 3)))
plot(data, pch = 16, col = rep(viridis::viridis(2), each = 150))
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
#plot(Test, pch = 16, col= glass$Type)


##Now Im trying the same procedure with the original Function

##Diferent eta plots
library(Rtsne)
par(mfrow=c(1,3))
test <-  Rtsne(data, perplexity = 30, learning_rate = 10)
plot(test$Y, pch = 16, col = iris$Species)
test2 <-  Rtsne(data, perplexity = 30, learning_rate = 100)
plot(test2$Y, pch = 16, col = iris$Species)
test3 <-  Rtsne(data, perplexity = 30, learning_rate = 1000)
plot(test3$Y, pch = 16, col = iris$Species)

##Diferent perp plots
library(Rtsne)
par(mfrow=c(1,6))
plot(data, pch = 16, col = rep(viridis::viridis(2), each = 200))
perp5 <-  tsne(data, perplexity = 5, learning_rate = 100)
plot(perp5, pch = 16, col = rep(viridis::viridis(2), each = 200))
perp10 <-  tsne(data, perplexity = 10, learning_rate = 100)
plot(perp10, pch = 16, col = rep(viridis::viridis(2), each = 200))
perp30 <-  tsne(data, perplexity = 30, learning_rate = 100)
plot(perp30, pch = 16, col = rep(viridis::viridis(2), each = 200))
perp50 <-  tsne(data, perplexity = 50, learning_rate = 100)
plot(perp50, pch = 16, col = rep(viridis::viridis(2), each = 200))
perp100 <-  tsne(data, perplexity = 400, learning_rate = 100)
plot(perp100, pch = 16, col = rep(viridis::viridis(2), each = 200))





##For mixture of gaussians

library(mvtnorm)
data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)))
plot(data, pch = 16, col = rep(viridis::viridis(2), each = 150) ) 

test <-  Rtsne(data, perplexity = 50 )

plot(test$Y, pch = 16,col = rep(viridis::viridis(2), each = 150) )




data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)),
              mvtnorm::rmvnorm(n = 150, mean = c(10, 10)))
plot(data, pch = 16, col = rep(viridis::viridis(3), each = 150))
Test <- tsne(data, perplexity = 50, learning_rate = 200, momentum = 0.5,
             cols = rep(viridis::viridis(3), each = 150), T = 1e4)
plot(test$Y, pch = 16,col = rep(viridis::viridis(3), each = 150) )


##With the glass data

glass <- read.csv("C:/Users/Alfonso/Desktop/glass.csv")
data <- as.matrix(unique(glass))
pr <- Rtsne(data,perplexity = 35,momentum = 0.5)
plot(pr$Y, pch = 16, col = glass$Type)


##Subplots in R

test <-  Rtsne(as.matrix(unique(iris[,-5])), perplexity = 35, eta = 10)
par(mfrow=c(3,1))
plot(test$Y, pch = 16, col = iris$Species)
test <-  Rtsne(as.matrix(unique(iris[,-5])), perplexity = 35, eta = 100)
plot(test$Y, pch = 16, col = iris$Species)
test <-  Rtsne(as.matrix(unique(iris[,-5])), perplexity = 35, eta = 1000)
plot(test$Y, pch = 16, col = iris$Species)



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
plot(data, pch = 16, col = rep(viridis::viridis(2), each = 150))

ourTsne <- tsne(data, perplexity = 35, learning_rate = 200, momentum = 0.5,
             cols = rep(viridis::viridis(2), each = 150), T = 1e3)

plot(ourTsne,col = rep(viridis::viridis(2), each = 150), pch = 16)
RtSNE <-  Rtsne(data, perplexity = 35, eta = 200 )
plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(2), each = 150) )
## tres gaussianas, la del medio mayor sigma mismo tamaño

data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19)), 
              mvtnorm::rmvnorm(n = 150, mean = c(0, 0),
              sigma = rbind(c(10, 0.2), c(0.2, 10))),
              mvtnorm::rmvnorm(n = 150, mean = c(19,19)))
#data
plot(data2, pch = 16, col = rep(viridis::viridis(3), each = 150))
ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
               , T = 1e3)
plot(ourTsne,col = rep(viridis::viridis(3), each = 150), pch = 16)
RtSNE <-  Rtsne(data2, perplexity = 35, eta = 200 )

plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(3), each = 150) )

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


plot(data3, col = sort(s), pch = 16)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne,col = sort(s), pch = 16)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200 )

plot(RtSNE$Y, pch = 16,col = sort(s) )

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


plot(data3, col = sort(s), pch = 16)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne,col = sort(s), pch = 16)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200 )

plot(RtSNE$Y, pch = 16,col = sort(s) )







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

plot(data, pch = 16, col = rep(viridis::viridis(2), each = 150))

ourTsne <- tsne(data, perplexity = 35, learning_rate = 200, momentum = 0.5,
                cols = rep(viridis::viridis(2), each = 150), T = 1e3)

plot(ourTsne$Y_t,col = rep(viridis::viridis(2), each = 150), pch = 16)

RtSNE <-  Rtsne(data, perplexity = 35, eta = 200, Y_init = ourTsne$Y_0 )
plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(2), each = 150) )
## tres gaussianas, la del medio mayor sigma mismo tamaño

data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19)), 
               mvtnorm::rmvnorm(n = 150, mean = c(0, 0),
                                sigma = rbind(c(10, 0.2), c(0.2, 10))),
               mvtnorm::rmvnorm(n = 150, mean = c(19,19)))
#data
plot(data2, pch = 16, col = rep(viridis::viridis(3), each = 150))
ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = rep(viridis::viridis(3), each = 150), pch = 16)
RtSNE <-  Rtsne(data2, perplexity = 35, eta = 200,Y_init =ourTsne$Y_0  )

plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(3), each = 150) )

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


plot(data3, col = sort(s), pch = 16)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = sort(s), pch = 16)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200,Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 16,col = sort(s) )

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


plot(data3, col = sort(s), pch = 16)

ourTsne <- tsne(data3, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = sort(s), pch = 16)
RtSNE <-  Rtsne(data3, perplexity = 35, eta = 200, Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 16,col = sort(s) )


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
plot(data4,pch = 16, col =  rep(viridis::viridis(5), each = 150))
ourTsne <- tsne(data4, perplexity = 70, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 2)
plot(ourTsne$Y_t, pch =15, col =  rep(viridis::viridis(5), each = n))
RtSNE <-  Rtsne(data4, perplexity = 70, eta = 200, Y_init = ourTsne$Y_0)
plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(5), each = 150))


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

plot(data4,pch = 16, col =  rep(viridis::viridis(5), each = 150))
ourTsne <- tsne(data4, perplexity = 70, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 2)
plot(ourTsne$Y_t, pch =15, col =  rep(viridis::viridis(5), each = n))
RtSNE <-  Rtsne(data4, perplexity = 70, eta = 200, Y_init = ourTsne$Y_0)
plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(5), each = 150))





################### Segundo intento gaussianas 3d 3 dimensiones
### GAUSSIANAS EN 3D
#par(mar=c(1,1,1,1))
#par(mfrow=c(4,3))
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
plot(RtSNE$Y, pch = 16, col =  rep(viridis::viridis(3), each = n))


## 3 gaussianas una distinta dispersion


data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19,-19)), 
               mvtnorm::rmvnorm(n = 150, mean = c(0, 0,0),
          sigma = rbind(c(1, 0.2,0.2), c(0.2, 1,0.2), c(0.2, 0.2, 1))),
               mvtnorm::rmvnorm(n = 150, mean = c(19,19,19)))
#data
scatterplot3d(data2, pch = 16, color = rep(viridis::viridis(3), each = 150))

ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = rep(viridis::viridis(3), each = 150), pch = 16)

RtSNE <-  Rtsne(data2, perplexity = 35, eta = 200,Y_init = ourTsne$Y_0  )

plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(3), each = 150) )


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


scatterplot3d(data3, color = sort(s), pch = 16)

ourTsne <- tsne(data3, perplexity = 50, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = sort(s), pch = 16)
RtSNE <-  Rtsne(data3, perplexity = 50, eta = 200,Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 16,col = sort(s) )

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


scatterplot3d(data4, color = sort(s), pch = 16)


ourTsne <- tsne(data4, perplexity = 50, learning_rate = 200, momentum = 0.5
                , T = 1e3)

plot(ourTsne$Y_t,col = sort(s), pch = 16)
RtSNE <-  Rtsne(data4, perplexity = 50, eta = 200, Y_init = ourTsne$Y_0 )

plot(RtSNE$Y, pch = 16,col = sort(s) )

#Iris segundo intento con y init
library(Rtsne)
data <- as.matrix(unique(iris[,-5]))
Test <- tsne(data,perplexity = 40, learning_rate = 200, momentum = 0.5, T= 2e3, 
             cols = iris$Species)


plot(Test$Y_t, col = iris$Species,pch = 16)
RtSNE <- Rtsne(data, perplexity = 40, eta = 200, Y_init = Test$Y_0)
plot(RtSNE$Y, col = iris$Species, pch = 16)





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

### Iteraciones final Stage the tsne versoin
par(mfrow=c(5,3))
par(mar=c(1,1,1,1))

data <- as.matrix(unique(iris[,-5]))
plot(data, pch = 16, col= iris$Species)
OurTsne <- tsne(data,perplexity = 33, learning_rate = 200,q = 2, momentum = 0.5, 
                T= 2800, cols = iris$Species)


### Segundo intento de gaussianas
### GAUSSIANAS 4D con diferentes varianzas


n= 400
data4 <- rbind(mvtnorm::rmvnorm(n = n, mean = c(-19, -19,4,20),
    sigma = rbind(c(3, 0.2,0.2,0.2), c(0.2, 3,0.2,0.2), 
                  c(0.2, 0.2, 3,0.2), c(0.2, 0.2,0.2,3) )                               
                                ), 
               mvtnorm::rmvnorm(n = n, mean = c(0, 0,-20,-15)),
               mvtnorm::rmvnorm(n = n, mean = c(19,19,20,0),
                sigma = rbind(c(3, 0.2,0.2,0.2), c(0.2, 3,0.2,0.2), 
                  c(0.2, 0.2, 3,0.2), c(0.2, 0.2,0.2,3) )      
                                
                                ))

par(mfrow=c(1,2))
pairs(data4, col =  rep(viridis::viridis(3), each = 400))


ourTsne <- tsne(data4, perplexity = 100, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 3)

scatterplot3d(ourTsne$Y_t, pch=15, color = rep(viridis::viridis(3), each = 400))

RtSNE <- Rtsne(data4,dims = 3,perplexity = 35, Y_init = ourTsne$Y_0,eta = 200 )

scatterplot3d(RtSNE$Y, pch = 16, color =  rep(viridis::viridis(3), each = 400))





#######################

credit <- read.csv(("C:/Users/Alfonso/Desktop/creditcard.csv"))
credit_rtsne <- Rtsne(unique(credit[,1:30]),perplexity = 200,eta = 1000)
plot(credit_rtsne$Y, col = credit$Class +3,pch = 16)


wine <- read.csv("C:/Users/Alfonso/Desktop/wine.csv")
wine_rtsne <- Rtsne(unique(wine),perplexity = 1000)
qu <- wine$quality>5.5
plot(wine_rtsne$Y, col = qu+1,pch = 16)

zoo <- read.csv("C:/Users/Alfonso/Desktop/zoo.csv")
zoo1 <- zoo[,-1]
zoo_rtsne <- Rtsne(unique(zoo1), perplexity = 17)
plot(zoo_rtsne$Y,col = zoo$class_type,pch = 16)

glass <- read.csv("C:/Users/Alfonso/Desktop/glass.csv")
glass_rtsne <- Rtsne(unique(glass))
qu <- glass$type
plot(glass_rtsne$Y, col = glass$Type
     ,pch = 16)





wine2 <- read.csv("C:/Users/Alfonso/Desktop/wine2.csv")
wine2_rtsne <- Rtsne(unique(wine2),perplexity = 14, dims = 2 )
plot(prcomp(wine2)$x,col = wine2[,1],pch = 16)
plot(wine2_rtsne$Y, col = wine2[,1],pch = 16)




breast <- read.csv("C:/Users/Alfonso/Desktop/breast.csv")
breast_rtsn <- Rtsne(unique(breast[1:9]),perplexity =25)
x = as.matrix(breast[1:9])
plot(prcomp(x)$x,color = breast[,10],pch = 16)

plot(breast_rtsn$Y, col = breast[,10],pch = 16)


heart <- read.csv("C:/Users/Alfonso/Desktop/heart.csv")
heart_rtsn <- Rtsne(unique(heart[1:13]),perplexity = 30
                    , eta= 1000)
x = as.matrix(heart[1:13])
plot(prcomp(x)$x,col = heart[,14]+1,pch = 16)

plot(heart_rtsn$Y, col = heart[,14] +1,pch = 16)




lung <- read.csv("C:/Users/Alfonso/Desktop/lung.csv")
set.seed(123456)
lung_rtsn <- Rtsne(unique(lung[2:57]),perplexity = 10)

plot(color = lung[,1],pch = 16)

plot(lung_rtsn$Y, col = lung[,1] ,pch = 16)




parkinsons <- read.csv("C:/Users/Alfonso/Desktop/parkinsons.csv")
set.seed(123456)
p1<- parkinsons[,-18]
p2<- p1[,-1]
parkinsons_rtsn <- Rtsne(unique(parkinsons[2:24]),perplexity = 30,eta = 1000)
parkinsons_rtsn <- Rtsne(p2,perplexity = 30,eta = 1000)
a<-prcomp(p2
          )
plot(a$x,col = parkinsons$status+1,pch= 16)

parkinsons_rtsn <- Rtsne(a$x,perplexity = 60)


plot(parkinsons_rtsn$Y, col = rep(rainbow(3),each = 10),pch = 16)






abs <- read.csv(("C:/Users/Alfonso/Desktop/abss.csv"))
abs_rtsne <- Rtsne(unique(abs[1:20]),perplexity = 200)
qu<- abs$Absenteeism.time.in.hours>9
qu <- abs$Son>3
plot(abs_rtsne$Y, col = qu +2,pch = 16)



sls <- read.csv(("C:/Users/Alfonso/Desktop/sls.csv"))
slsrt <- Rtsne(sls,perplexity = 30)
plot(slsrt$Y,col = sls$Region)


io <- read.csv(("C:/Users/Alfonso/Desktop/io.csv"))
iort <- Rtsne(unique(io[1:34]),perplexity = 35)
plot(iort$Y,col = io[,35],pch =16)


elec <- read.csv(("C:/Users/Alfonso/Desktop/elec.csv"))
elecrt <- Rtsne(unique(elec[,1:11]),perplexity = 10, eta = 1000)
plot(elecrt$Y,col = elec[,12]+1,pch =16)



hep <- read.csv(("C:/Users/Alfonso/Desktop/hep.csv"))
heprt <- Rtsne(unique(hep[,1:19]),perplexity = 10)
plot(heprt$Y,col = hep[,20],pch =16)




card <- read.csv(("C:/Users/Alfonso/Desktop/card.csv"))
cardrt <- Rtsne(unique(card[1:19]),perplexity = 10)
plot(cardrt$Y,col = card[,20],pch =16)






