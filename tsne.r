#Script Tsne
#ALFONSO ALBACETE ZAPATA

data <- iris[,1:2]
#Cargar la data = iris[,-5]
#Sin la columna de las especiess(nombres)
tsne<- function(data){

  x = data
  n = nrow(data)
  p = ncol(data)
  
  #set initial parameters
  learningrate = 100
  #Establecemos estos valores iniciales
  momentum  = 0.5
  #momentum = 0.8
  perplexity = 10
  
  #Set initial configuration of the low dimensional
  #data
  #number of dimensions we want to reduce to.
  sigma = 1/sqrt(2)
  q = 2
  ydata = matrix(data = rnorm(n = (n * p), mean = 0, sd = sigma), nrow = n, ncol = q)
  
  #calculating the conditional probabilities in high dimension:
  
  P = matrix(data = 0, nrow = n, ncol = p)
  
  P = as.matrix(exp((-dist(x, diag = TRUE) ^ 2) / (2 * sigma ^ 2)),nrow = n, ncol = p)
  
  #exceptuating the diagonal
  v = rowSums(P) - 1
  
  p_icondj = P / v
  
  p_ij = (p_icondj + t(p_icondj)) / (2 * n)
  
  p_jcondi = t(p_icondj)
  
  # I have found a trouble computing the perplexity:
  # Main trouble is computing the log of 0, since the diag is 0
  uno = (p_jcondi) * log2(p_jcondi + 1)
  entropy = -colSums(uno)
  perplexity = 2 ^ (entropy)
  #Este valor deberia dar entre 5 y 50 para un correcto desarrollo
  perplexity = 30
  
  #calculating the conditional probabilities in low dimension:
  
  Q = matrix(data = 0, nrow = n, ncol = q)
  
  Q = as.matrix(exp((-dist(ydata, diag = TRUE) ^ 2)),nrow = n, ncol = q)
  
  #The 1 comes from the diagonal
  v2 = rowSums(Q) - 1
  
  q_icondj = Q / v2
  
  q_ij = (q_icondj + t(q_icondj)) / (2 * n)
  
  q_jcondi = t(q_icondj)
  
  #Computing the gradient
  #Para hacer la inversa de esta matriz utilizo la funcion solve 
  inversa = as.matrix((1+ (dist(ydata, diag = TRUE)) ^ 2))
  inversa = solve(inversa)
  #ARREGLAR EL GRADIENTE
  gradient = matrix(data = 0, nrow = n, ncol = q)
  # tot = vector(mode = "list", length = n)
  # for(i in tot) {
  #  gradient[i,] = 4 * colSums( (p_ij - q_ij) * (ydata - ydata[,i]) * inversa)
  # }
  #Probar de esta forma
  #apply con 1 es row
  #aaply con 2 es column
  stiffnesses = 4 * (p_ij - q_ij) * inversa 
  #El problema está en esta funcion
  #No actualiza bien el gradiente de manera que no se repelen bien los puntos

  #Para coger las columnas y filas de ydata: 
  #Mirar documentacion pero como en matlab: ydata[1,]
  #Once we have computed everything:
  
  #Dejo directamente la del comentario
  Y_t =  ydata
  Y_t_1 = ydata
  Y_t_2 = ydata
  
  #Ycalc1 is Ycalc in the previous step
  #ycalc2 is Ycalc 2 steps before
  #Te = vector(mode = "list", length = 100)
  Te = as.matrix(1:1000)
   
  for (t in Te) {
    
    Q = as.matrix(exp((-dist(Y_t, diag = TRUE) ^ 2)),nrow = n, ncol = q)
    #The 1 comes from the diagonal
    v2 = rowSums(Q) - 1
    q_icondj = Q / v2
    q_ij = (q_icondj + t(q_icondj)) / (2 * n)
    inversa = as.matrix((1+ (dist(Y_t, diag = TRUE)) ^ 2))
    inversa1 = solve(inversa)
    stiffnesses = 4 * (p_ij - q_ij) * inversa1 
    
    for (i in 1:n){
      
      gradient[i,] = apply(sweep(Y_t, 2, -Y_t[i,]) * stiffnesses[,i] ,2,sum)
      
    }
    
    Y_t = Y_t_1 + learningrate * gradient + momentum * (Y_t_1 - Y_t_2)

    #Y_t = Y_t_1 + 50 * gradient + 0.5 * (Y_t_1 - Y_t_2)
    
    
    if(t > 700){
      #learningrate = learningrate - 1
      #Mayor learning rate menor los resultados, menor learning mas grandes
      momentum = 0.8
    }
    Y_t_2 = Y_t_1  
    Y_t_1 = Y_t
    message("Iteration #",t)
    
    if((i%%100)==0){
      plot(Y_t, pch =15 , col = iris$Species)
    }
    
  }
  
  Y_t
  return(Y_t)
  
}


# Para plotear los resultados
#data <- iris[,-5]
# tsne1 <- tsne(data)
#
#plot(tsne1, pch = 15, col = iris$Species)






