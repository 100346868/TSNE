###############################################################################
#THIS IS TSNE IMPLEMENTED BY ALFONSO ALBACETE
#FOR HIS FINAL DEGREE PROJECT
###############################################################################

#This functions will compute the softmax function in a stabilized way

logsumexp <- function (x) {
  y = max(x)
  y + log(sum(exp(x - y)))
}

softmax <- function (x) {
  exp(x - logsumexp(x))
}

#This function is in charge of computing the sigma optimal parameters to
# calculate the P_{ij} Matrix with the values adapted to the perplexity fixed 
#by argument

calcP <- function(X, perplex){
  
  n = nrow(X)
  sigma2_opt = rep(1,n)
  
  sigma2_opt <- sapply(1:n, function(i) {
    
    optim(par = 0.75, 
          fn = function(s2) {
            res <- (calc_perplexity(X = X, i = i, sigma2 = s2) - perplex)^2
            ifelse(is.finite(res), res, 1e6)
          },
          method = "L-BFGS-B", lower = 0.1)$par
    
  })
  
  
  P_i_cond_j= matrix(0,n,n)
  P_i_cond_j <- sapply(1:n, function(i) {
    
    softmax(-rowSums( t(t(X) - X[i, ])^2) / (2 * sigma2_opt[i]))
    
  })
  
  P_ij = (P_i_cond_j + t(P_i_cond_j)) / (2 * n)
  return(P_ij)
  
}

#Computes the perplexity for the sigma^2 given for a particular i

calc_perplexity <-function(X, i, sigma2) {
  
  # Dimensions
  n <- nrow(X)
  p <- ncol(X)
  
  P_i_cond_j= softmax(- rowSums( t(t(X) - X[i, ])^2) / (2 * sigma2))
  H_i <- -sum(P_i_cond_j * log2(P_i_cond_j))
  
  #returns the perplexity
  return(2^H_i)
  
}


##TSNE ALGORITHM


tsne <- function(X, q = 2, T = 1e3, learning_rate = 100, momentum = 0.5, 
                 perplexity = 35, cols = 1){
  
  # Dimensions
  n = nrow(X)
  p = ncol(X)
  
  ### This P is calculated using the perplexity on the parameter
  ## the sigma is calculated to obtain the perplexity fixed
  
  P_ij = calcP(X, perplex = perplexity ) 
  
  
  # Initial configuration of the low dimensional data
  set.seed(123456)
  Y_t = mvtnorm::rmvnorm(n = n, mean = rep(0, q), sigma = diag(rep(1e-4, q)))
  Y_0 =  Y_t
  Y_t_1 = Y_t_2 = Y_t
  
  # Loop
  gradient <- matrix(0, nrow = n, ncol = q)
  for (t in 1:T) {
    
    ## Compute the low-dimensional affinities q_{ij}'s
    
    # (1 + ||y_i - y_j||^2)^{-1}
    Q <- as.matrix(1 / (1 + dist(Y_t)^2))
    
    # Q_ij
    # Sum for rows, except for the diagonal
    v = sum(Q)
    
    # Quotient with an implicit column recycling
    Q_ij = Q / v
    
    ## Gradient    
    
    # p_ij - q_ij
    dif_P_Q <- P_ij - Q_ij
    
    # Fill gradient
    for (i in 1:n) {
      gradient[i, ] = 4 * colSums(dif_P_Q[i, ] * t(t(Y_t) - Y_t[i, ]) * Q[i, ])
      
    }
    
    ## Update the Y following the algorithm described in the paper
    
    Y_t = Y_t_1 + learning_rate * gradient + momentum * (Y_t_1 - Y_t_2)
    Y_t_2 = Y_t_1  
    Y_t_1 = Y_t
    
    if(t > 700){
      momentum = 0.8
    }
    #There is a plot each 200 iterations to show the development of the 
    #algorithm
    
    
    
  }
  
  p = {}
  p$Y_0 = Y_0
  p$Y_t = Y_t
  p
  #return(Y_t)
  ## Devuelve la inicialización
}


