# Función para calcular el factorial utilizando sumas sucesivas
factorial2 <- function(n) {
  # Función interna para calcular factorial recursivamente
  calcular_factorial <- function(k) {
    if (k == 0) {
      return(1)  # Caso base: factorial de 0 es 1
    } else {
      return(sum_sec_rec(k, calcular_factorial(k - 1)))  # Calcula factorial de k recursivamente
    }
  }

  # Llama a la función interna para calcular el factorial de n
  return(calcular_factorial(n))
}

#multiplicación por sumas sucesivas
sum_sec_rec <- function(b,a){
  if(b==0){
    return(0)
  }else{
    return(a + sum_sec_rec(b-1,a))
  }
}

#potencia por multiplicaciones sucesivas
potencia_suc_rec_ <- function(base, exponente){
  if(exponente==0){
    return(1)
  }else{
    return(sum_sec_rec(base,potencia_suc_rec_(base,exponente-1)))
  }
}

#recursividad
factotial1 <- function(N){
  f <- 1
  i <- 1
  while(i<=N){
    f <- f*i
    i <- i + 1
  }
  return(f)
}
f_recursiva <- function(N){
  if(N==0){
    return(1)
  }else{
    return(N*f_recursiva(N-1))
  }
}
