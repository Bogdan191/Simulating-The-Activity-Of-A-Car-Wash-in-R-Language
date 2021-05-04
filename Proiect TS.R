
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
    
}

#definire functia de repartitie G1(x) pentru Y1
g1 <- function(x){
  
}



  