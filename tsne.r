  #Script Tsne
  #ALFONSO ALBACETE ZAPATA
  data <- iris
  
  
  tsne<- function(data){
    #Firstly we process the data
    data = iris
    x =data
    n = nrow(data)
    p = ncol(data)
    #set initial parameters
    learningrate = 0.23
    momentum  =0.343
    perplexity = 5 
    
    #Set initial configuration of the low dimensional
    #data
    #number of dimensions we want to reduce to.
    sigma = 1/sqrt(2)
    q = 2
    ydata = matrix(data = rnorm(n = (n * p), mean = 0, sd = sigma), 
                   nrow = n, ncol = q)
    
    #calculating the conditional probabilities in high dimension:
  
    P = matrix(data = 0, nrow = n, ncol = p)
    
    P = as.matrix(exp((-dist(x, diag = TRUE)^2)/2*sigma^2))
    
    v = rowSums(P) - 1
    
    p_icondj = P * v
    
    p_ij = (p_icondj + t(p_icondj))/2*n
    
    p_jcondi = t(p_icondj)
    perplexity = 2^(-colSums(p_jcondi*log2(p_jcondi)))
    
    #calculating the conditional probabilities in low dimension:
    
    Q = matrix(data = 0, nrow = n, ncol = p)
    
    Q = as.matrix(exp((-dist(ydata, diag = TRUE)^2)))
    
    #The 1 comes from the diagonal
    v2 = rowSums(Q) - 1
    
    q_icondj = Q * v2
    
    q_ij = (q_icondj + t(q_icondj))/2*n
    
    q_jcondi = t(q_icondj)
    
   inversa = as.matrix((1+ abs(dist(ydata, diag = TRUE))^2)^-1)
    #Computing the gradient
   gradient = 4*colSums( (p_ij - q_ij)*(as.matrix(dist(ydata,diag= TRUE)))*inversa )
    
    #Once we have computed everything:
    T = vector(length = 10000)
    
    Ycalc = matrix(data = ydata,nrow = row(ydata) ,ncol = col(ydata))
    Ycalc1 = matrix(data = ydata,nrow = row(ydata) ,ncol = col(ydata))
    Ycalc2 = matrix(data = ydata,nrow = row(ydata) ,ncol = col(ydata))
    #gamma1 is gamma in the previous step
    #gamma2 is gamma 2 steps before
  
    for (t in T) {
      Ycalc = Ycalc1 + learningrate*gradient + momentum*(Ycalc1 - Ycalc2)
      Ycalc2 = Ycalc1
      Ycalc1 = Ycalc
      plot(Ycalc)
      Ycalc
      
    }
    
  
  }










