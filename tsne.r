#Script Tsne
#ALFONSO ALBACETE ZAPATA


#Cargar la data = iris[,-5]
#Sin la columna de las especies
tsne<- function(data){

  x = data
  n = nrow(data)
  p = ncol(data)
  
  #set initial parameters
  learningrate = 100
  #Establecemos estos valores iniciales
  momentum  = 0.5
  #momentum = 0.8
  perplexity = 30
  
  #Set initial configuration of the low dimensional
  #data
  #number of dimensions we want to reduce to.
  sigma = 1/sqrt(2)
  q = 2
  ydata = matrix(data = rnorm(n = (n * p), mean = 0, sd = sigma), nrow = n, ncol = q)
  
  #calculating the conditional probabilities in high dimension:
  
  P = matrix(data = 0, nrow = n, ncol = p)
  
  P = as.matrix(exp((-dist(x, diag = TRUE) ^ 2) / (2 * sigma ^ 2)))
  
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
  
  Q = matrix(data = 0, nrow = n, ncol = p)
  
  Q = as.matrix(exp((-dist(ydata, diag = TRUE) ^ 2)))
  
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
  for (i in 1:n){
    gradient[i,] = apply(sweep(-ydata, 2, -ydata[i,]) * stiffnesses[,i],2,sum)
  }
  
  
  #Para coger las columnas y filas de ydata: 
  #Mirar documentacion pero como en matlab: ydata[1,]
  #Once we have computed everything:
  
  #Dejo directamente la del comentario
  Ycalc =  ydata
  Ycalc1 = ydata
  Ycalc2 = ydata
  #Ycalc1 is Ycalc in the previous step
  #ycalc2 is Ycalc 2 steps before
  Te = vector(mode = "list", length = 100)
  Te = as.matrix(1:1000)
   
  for (t in Te) {
    
    Ycalc = Ycalc1 + learningrate * gradient + momentum * (Ycalc1 - Ycalc2)
    #Ycalc = Ycalc1 + 100 * gradient + 0.8 * (Ycalc1 - Ycalc2)
    learningrate = learningrate +1;
    Ycalc2 = Ycalc1  
    Ycalc1 = Ycalc
    message("Iteration #",t)
    
  }
  
  Ycalc
  return(Ycalc)
  #Como resultado final se está agrupando pero se está agrupando
  #todo en el mismo sitio, no se separa en grupos.
  
}









