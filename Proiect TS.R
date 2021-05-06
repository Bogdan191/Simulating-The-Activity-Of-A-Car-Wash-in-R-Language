
#Pintilie Bogdan-Ioan, Proiect Tehnici de simulare
#Simularea activitatii unei spalatorii cu auto-servire pentru masini




#Generare lambda t in functie de valoarea lui t
genLambdaT <- function(t) {
  
  if(t >= 0 && t <= 2)
    return (3)
  if(t < 6) 
    return (tan(0.2*t) + sin(2*(t^2))^2 + 4)
  
  if(t <= 9)
    return (7)
  
  if(t <= 12)
    return (t + 1)
  
  return (t)
    
}

# Generare variabila TS

genTs <- function(s) {
  t <- s
  u1 <- runif(1, 0, 1)
  u <- runif(1,0, 1.2)
  
  lambdaT <- genLambdaT(t)
  print(lambdaT)
  
  lambda <- round(lambdaT)
  print(lambda)
  
  while(u < lambdaT/lambda) {
    u1 <- runif(1, 0, 1)
    u <- runif(1, 0, 1.2)
    
    t <- t-(1/lambda) * log(u1)
    
  }
  
  return(t)
  
}

#   definire functia de repartitie G1(x) pentru Y1, Y1 fiind timpult de 
# servire la serverul 1 
g1 <- function(x){
  if(x < 3)
    return (0)
  
  if(x >=3 && x < 7)
    return (0.2)
  
  if(x >=7 && x < 9)
    return (0.4)
  
  return(1)
}

x <- genTs(2)
print(x)
#Generare Y1
genY1 <- function() {
  
  x <- runif(1, 0, 12)
  
  return (g1(x))
}


# definire Y2(timpul de servire la serverul 2)

pois <- function(x) {
  
  i <- 0
  P <- 1
  
  while(P < exp(-x)) {
    U <- runif(0, 1)
    i <- i + 1
    P <- P * U
  }
  X <- i - 1
  return (X)
  
  
  
}

norm <- function(x, ab_med) {
 
  return (0)
    
}


genY2 <- function() {
  
  x2 <- pois(5)
  x3 <- norm(4, 0.8)
  
  if(3 > x2 && 3 > x3)
    return(3)
  
  if(x2 > 3 && x2 > x3)
    return(x2)
  
  return(x3)
}


main <- function() {
  
  # Initializare Variabile 
  T <- 0 # variabila de timp
  SS <- c(0, 0, 0) # sistem sub forma de (n, i1, i2), unde:
                                        # n - nr de clienti din sistem
                                        # i1 - clientul curent la serverul 1
                                        # i2 - clientul curent la serverul 2
  TA <- genTs(0) # timpul la care va sosi urmatorul client
  T1 <- Inf # timpul la care clientul iese din primul server
  T2 <- Inf # timpul la care clientul paraseste cel de al 2-lea server
  Na <- 0 # nr de clienti total la un mom dat
  C1 <- 0 # nr total de clienti serviti de serverul 1
  C2 <- 0 # nr total de clienti serviti de al 2-lea server
  A <- list()# lista cu timpii la care sosesc clientii 
  D <- list() # lista cu timpii la care clientii parasesc sistemul
  
  nrOrd <- 0 # numarul de ordine din sistem
  server1 <- list() # se salveaza pentru fiecare client, timpul de servire pentru serverul 1 
  server2 <- list()  #se salveaza pentru fiecare client, timpul de servire in cadrul celui de al doilea server
  
  
  while(T < 1000) { 
    
    # Primul caz: cand soseste clientul verificam daca poate fi servit imediat
    # sau daca intra in coada de asteptare
    if(TA == min(TA, T1, T2)) {
        T = TA
        Na = Na + 1
        TA = genTs(T)
        
        A[Na] = T
        
        if(SS[1] <= 1) {  # un singur client in sistem
          
          # verific dc sistemul este gol, i.e. nici un client in sistem
          if(SS[1] == 0 & SS[2] == 0 & SS[3] == 0 ){
            # adaugam clientul direct in sistem, pe prima pozitie
            SS[1] = 1
            SS[2] = Na
            Y1 = genY1()
            T1 = T + Y1
          }else if(SS[1]== 1 & SS[2] != 0 & SS[3] == 0) { # serverul 1 ocupat
            
            SS[1] = 2
            SS[3] = Na
            Y2 = genY2()
            T2 = T + Y2
            
          }else if(SS[1] == 1 & SS[2] == 0 && SS[3] != 0){ # serverul 2 ocupat
            SS[1] = 2
            SS[2] = Na
            Y1 = genY1()
            T1 = T + Y1
          }
         
        } else # Mai multi clienti in sistem
          {
            SS[1] = SS[1] + 1
          }
    
    }
    
    # Caz 2: Serverul 1 se elibereaza inainte de sosirea unui nou client
    if(T1 < TA & T1 <= T2){
      T = T1
      C1 = C1 + 1
      D[SS[2]] = T
      server1 = c(server1, T-unlist(A[SS[2]])) # adaugam timpul in care clientul
                      # n a stat in serverul 1
      
      if(SS[1] == 1) # avem doar un client in sistem 
      {
        SS[1] = 0
        SS[2] = 0
        SS[3] = 0
        T1 = Inf
      } else if(SS[1] == 2){ #In sistem sunt 2 clienti
        SS[1] = 1
        SS[2] = 2
        T1 = Inf
        
      } else if(SS[1] > 2){# In sistem sunt cel putin 3 clienti
        
          nrOrd = max(SS[2], SS[3]) # nrOrd  + 1 reprezinta numarul de ordine pt client nou
          SS[1] = SS[1] - 1
          SS[2] = nrOrd + 1
          Y1 = genY1()
          T1 = T + Y1
        
      }  
      
    }
    
    # Caz 3. Server 2 liber inaintea serverului 1
    if(T2 < TA && T2 < T1) {
      T = T2 
      C2 = C2 + T
      D[SS[3]] = T
      server2 = c(server2, T-unlist(A[SS[3]])) # adaugam timpul in care clientul
                                               # n a stat in serverul 2
      
      if(SS[1] == 1) # Doar un client in sistem
      {
        SS[1] = 0
        SS[2] = 0
        SS[3] = 0
        T2 = Inf
      }else if(SS[1] == 2){ # In sistem sunt 2 clienti
        SS[1] = 1
        SS[3] = 0
        T2 = Inf
        
      }else if(SS[1] > 2) {# Daca in sistem exista mai mult de 2 clienti 
        nrOrd = max(SS[2], SS[3]) 
        SS[1] = SS[1] - 1
        SS[3] = nrOrd + 1
        Y2 = genY2()
        T2 = T + Y2
        
        
      }
        
      
    }
  
  }
  
  totalC <- min(length(A), length(D))
  
  timpPetrecut <- list()
  # se calculeaza timpul prntru fiecare cleint in sistem si salvam in lista definita mai sus
  
  for(i in seq(1, 1000)) {
    timpPetrecut <- c(timpPetrecut,unlist(D[i]) - unlist(A[i]))
    
  }
  
  hist(unlist(timpPetrecut), breaks = 100, main="Timp petrecut")
  
}

main()



  