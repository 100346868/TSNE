###############################################################################
#THIS IS TSNE IMPLEMENTED BY ALFONSO ALBACETE
#FOR HIS FINAL DEGREE PROJECT
###############################################################################

#In this script there are some test to try the algorithm

#Iris

data <- as.matrix(unique(iris[,-5]))
Test <- tsne(data,perplexity = 35, learning_rate = 200, momentum = 0.5, T= 1e4, 
             cols = iris$Species)

#Mixture of gaussians

data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)))
Test <- tsne(data, perplexity = 50, learning_rate = 200, momentum = 0.5,
               cols = rep(viridis::viridis(2), each = 150), T = 1e4)


#iris in 3d

data <- as.matrix(iris[,-5])
Test <- tsne(data,perplexity = 35, learning_rate = 200,q = 3, momentum = 0.5, 
             T= 1e3, cols = iris$Species)
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
scatterplot3d(Test, pch = 16, color=colors)

# Daring to something more ambicious
# Glass dataset: https://www.kaggle.com/uciml/glass 

glass <- read.csv("glass.csv")
data <- as.matrix(unique(glass))
Test <- tsne(data,perplexity = 35, learning_rate = 200,q = 2, momentum = 0.5, 
             T= 4e3, cols = glass$Type)
#plot(Test, pch = 15, col= glass$Type)


##Now Im trying the same procedure with the original Function

library(Rtsne)
test <-  Rtsne(as.matrix(unique(iris[,-5])), perplexity = 35)
plot(test$Y, pch = 15, col = iris$Species)


##For mixture of gaussians


data <- rbind(mvtnorm::rmvnorm(n = 150, mean = c(-3, -3)), 
              mvtnorm::rmvnorm(n = 150, mean = c(3, 3)))

test <-  Rtsne(as.matrix(unique(data)), perplexity = 35)

plot(test$Y, pch = 15,col = rep(viridis::viridis(2)) )

##With the glass data

glass <- read.csv("glass.csv")
data <- as.matrix(unique(glass))
pr <- Rtsne(data,perplexity = 35,momentum = 0.5)
plot(pr$Y, pch = 15, col = glass$Type)


