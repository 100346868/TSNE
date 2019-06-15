###################################
#Alfonso Albacete Zapata
#Implementation of the TFG
#####################################
install.packages("Rtsne")
install.packages("imager")
install.packages("scatterplot3d")
install.packages("viridis")
install.packages("mvtnorm")
install.packages("nlme")

library(Rtsne)
library(imager)
library(scatterplot3d)
library(viridis)
library(mvtnorm)
library(nlme)


#Different eta plots
data <- as.matrix(unique(iris[,-5]))
set.seed(123456)
n = nrow(unique(iris))

q = 2
Y_1 = mvtnorm::rmvnorm(n = n, mean = rep(0, q), sigma = diag(rep(1e-4, q)))

par(mfrow=c(1,3))
test <-  Rtsne(data, perplexity = 35, eta  = 1, Y_init = Y_1)
plot(test$Y, pch = 16, col = iris$Species)
test2 <-  Rtsne(data, perplexity = 35, eta = 10, Y_init = Y_1)
plot(test2$Y, pch = 16, col = iris$Species)
test3 <-  Rtsne(data, perplexity = 35, eta = 1000, Y_init = Y_1)
plot(test3$Y, pch = 16, col = iris$Species)


########################
# MNIST
# Takes over 20 mins
# Rtsne
mnist <- read.csv("C:/Users/Alfonso/Desktop/mnist.csv")

set.seed(123456)
data <- as.matrix(unique(mnist))
MnistRtsne <- Rtsne(data,dims = 2,perplexity = 100,verbose=TRUE,momentum = 0.5)
# Evitar color 0
m1 <- (mnist$label  +2)
plot(MnistRtsne$Y, pch = 16, col = mnist$label, xlab = 'X', ylab = 'Y'
     ,main = 'Rtsne on Mnist')


# PCA

MnistPCA <- prcomp(data)
plot(MnistPCA$x, pch =16, col = m1,
     ,main = 'PCA on Mnist' )

# Mnist Rtsne Over PCA
# Takes over 10 minutes
MnistPCARtsne <- Rtsne(MnistPCA$x,,dims = 2,
                       perplexity = 100 ,verbose=TRUE,momentum = 0.5)

plot(MnistPCARtsne$Y, pch = 16, col = mnist$label, xlab = 'X', ylab = 'Y'
     ,main = 'Rtsne on Mnist over PCA')

# MDS
#x <- cmdscale()




## IRIS 
pairs(iris[1:4],
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

# iris both version: Rtsne and our tsne

library(Rtsne)
par(mfrow=c(1,2))

data <- as.matrix(unique(iris[,-5]))
Test <- tsne(data,perplexity = 40, learning_rate = 200, momentum = 0.5, T= 2e3, 
             cols = iris$Species)

plot(Test$Y_t, col = iris$Species,pch = 16,  xlab = 'X', ylab = 'Y'
     ,main = 'Our t-SNE version on iris dataset')
RtSNE <- Rtsne(data, perplexity = 40, eta = 200, Y_init = Test$Y_0)
plot(RtSNE$Y, col = iris$Species, pch = 16, xlab = 'X', ylab = 'Y'
     ,main = 'Rtsne package over iris')


### Iteraciones final Stage the tsne version usando Iris

par(mfrow=c(5,3))
par(mar=c(1,1,1,1))

data <- as.matrix(unique(iris[,-5]))
plot(data, pch = 16, col= iris$Species)
OurTsne <- tsne(data,perplexity = 33, learning_rate = 200,q = 2, momentum = 0.5, 
                T= 2800, cols = iris$Species)



# Iris in 3D

par(mfrow=c(1,2))

data <- as.matrix(unique(iris[,-5]))
OurTsne <- tsne(data,perplexity = 35, learning_rate = 200,q = 3, momentum = 0.5, 
                T= 1e3, cols = iris$Species)
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
z<- as.matrix(colors)
scatterplot3d(OurTsne$Y_t, pch = 16, color=z[-150,], xlab = 'X', ylab = 'Y',
              zlab = 'Z',main ='Our t-SNE version' )

RtSNE = Rtsne(data, dims = 3, eta = 200, perplexity = 30, Y_init = OurTsne$Y_0 )
color = iris$Species
colors = colors[as.numeric(color)]

scatterplot3d(RtSNE$Y, pch = 16,color = z[-150,], xlab = 'X', ylab = 'Y',
              zlab= 'Z',main ='Rtsne package version')


# Applied to synthetic data

# Gaussianas in 2D

# dos gaussianas mismo tamaño separadas

set.seed(123456)
par(mfrow=c(1,3))
data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)))

plot(data, pch = 16, col = rep(viridis::viridis(2), each = 150))

ourTsne <- tsne(data, perplexity = 40, learning_rate = 200, momentum = 0.5,
                cols = rep(viridis::viridis(2), each = 150), T = 1e3)

plot(ourTsne$Y_t,col = rep(viridis::viridis(2), each = 150), pch = 16)

RtSNE <-  Rtsne(data, perplexity = 40, eta = 200, Y_init = ourTsne$Y_0 )
plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(2), each = 150) )
## tres gaussianas, la del medio mayor sigma mismo tamaño
par(mfrow=c(1,3))

data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-19, -19)), 
               mvtnorm::rmvnorm(n = 150, mean = c(0, 0),
                                sigma = rbind(c(10, 0.2), c(0.2, 10))),
               mvtnorm::rmvnorm(n = 150, mean = c(19,19)))
#data
plot(data2, pch = 16, col = rep(viridis::viridis(3), each = 150))
ourTsne <- tsne(data2, perplexity = 40, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = rep(viridis::viridis(3), each = 150), pch = 16)
RtSNE <-  Rtsne(data2, perplexity = 40, eta = 200,Y_init =ourTsne$Y_0  )

plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(3), each = 150) )

## 4 gaussianas diferentes tamaños
#Gaussianas diferentes tamaños
set.seed(123546)
par(mfrow=c(1,3))
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
par(mfrow=c(1,3))
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
plot(RtSNE$Y, pch = 16,col = sort(s))


### GAUSSIANAS EN 3D

# Same size
par(mfrow=c(1,3))

n= 300
data2 <- rbind(mvtnorm::rmvnorm(n = n, mean = c(-10, -10,4)), 
               mvtnorm::rmvnorm(n = n, mean = c(0, 0,-15)),
               mvtnorm::rmvnorm(n = n, mean = c(10,10,15)))


scatterplot3d(data2, color  = rep(viridis::viridis(3), each = 300))
ourTsne <- tsne(data2, perplexity = 50, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 2)
plot(ourTsne$Y_t, pch = 16, col =  rep(viridis::viridis(3), each = n))
RtSNE <- Rtsne(data2,dims = 2, perplexity = 50, Y_init = ourTsne$Y_0)
plot(RtSNE$Y, pch = 16, col =  rep(viridis::viridis(3), each = n))


## 3 gaussianas una distinta dispersion
par(mfrow=c(1,3))

data2 <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-10, -10,-10)), 
               mvtnorm::rmvnorm(n = 150, mean = c(0, 0,0),
                                sigma = rbind(c(3, 0.2,0.2), c(0.2, 3,0.2), c(0.2, 0.2, 1))),
               mvtnorm::rmvnorm(n = 150, mean = c(10,10,10)))
#data
scatterplot3d(data2, pch = 16, color = rep(viridis::viridis(3), each = 150))

ourTsne <- tsne(data2, perplexity = 35, learning_rate = 200, momentum = 0.5
                , T = 1e3)
plot(ourTsne$Y_t,col = rep(viridis::viridis(3), each = 150), pch = 16)

RtSNE <-  Rtsne(data2, perplexity = 35, eta = 200,Y_init = ourTsne$Y_0  )

plot(RtSNE$Y, pch = 16,col = rep(viridis::viridis(3), each = 150) )


## 4 gaussianas diferentes tamaños
# gaussianas diferentes tamaños
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

## GAUSSIANAS IN 4D




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


pairs(data4, col =  rep(viridis::viridis(3), each = 400))

par(mfrow=c(1,2))

ourTsne <- tsne(data4, perplexity = 100, learning_rate = 200, momentum = 0.5
                , T = 1e3, q = 3)

scatterplot3d(ourTsne$Y_t, pch = 16, color = rep(viridis::viridis(3), each = 400))

RtSNE <- Rtsne(data4,dims = 3,perplexity = 35, Y_init = ourTsne$Y_0,eta = 200 )

scatterplot3d(RtSNE$Y, pch = 16, color =  rep(viridis::viridis(3), each = 400))



### REAL APPLICATION

# Glass dataset
set.seed(123456)
glass <- read.csv("C:/Users/Alfonso/Desktop/glass.csv")
pr <- Rtsne(unique(glass[1:9]),perplexity = 10,momentum = 0.5, eta =1000)
plot(pr$Y, pch = 16, col = glass$Type, xlab = 'X',ylab = 'Y'
     ,main= 'Rtsne on glass dataset')





# http://archive.ics.uci.edu/ml/datasets/Wine
set.seed(123456)
wine2 <- read.csv("C:/Users/Alfonso/Desktop/wine2.csv")
wine2_rtsne <- Rtsne(unique(wine2[2:13]),perplexity = 15 , dims = 2, eta = 700 )
plot(wine2_rtsne$Y, col = wine2[,1],pch = 16,xlab = 'X'
     ,ylab = 'Y',main = 'Rtsne on Wines')


## Heart disease
set.seed(123456)
heart1 <- read.csv("C:/Users/Alfonso/Desktop/heart1.csv")
heart1_rtsne <- Rtsne(heart1[,-1],perplexity =50
                      , dims = 2, eta =1000)
plot(heart1_rtsne$Y, col = heart1[,1]+1,pch = 16)


## fruits

co = list.files("C:/Users/Alfonso/Desktop/frutas", pattern = "*.jpg")

x = matrix(0,nrow = 62500, ncol = 180)
for (i in 1:180){
  a <-paste0("C:/Users/Alfonso/Desktop/frutas/",co[i])
  im <- load.image(a)
  gr<- grayscale(im)
  s <- as.vector(gr)
  x[,i] = s
}

set.seed(123456)
test <- Rtsne( unique(t(x)), perplexity = 50,eta = 500, dims =2 )
plot(test$Y,col = rep(1:3, each = 60),pch =16,main= 'Rtsne on fruits',
     xlab = 'X', ylab = 'Y')

## Ionospherre radar

set.seed(123456)
io <- read.csv(("C:/Users/Alfonso/Desktop/io.csv"))
iort <- Rtsne(unique(io[1:34]),perplexity = 20, eta = 1000)
plot(iort$Y,col = io[,35],pch =16,xlab='x',
ylab='y',main='Rtsne on Ionosphere dataset')


## Electrocardiogram

set.seed(12346)
elec <- read.csv(("C:/Users/Alfonso/Desktop/elec.csv"))
elecrt <- Rtsne(unique(elec[1:11]),perplexity = 10, eta = 1000)
plot(elecrt$Y,col = elec[,12]+1,pch =16, xlab = 'X',
     ylab = 'Y', main = 'Rtsne applied on electrocardiogram')
#1 vivos rojos
#0 muertos negros



