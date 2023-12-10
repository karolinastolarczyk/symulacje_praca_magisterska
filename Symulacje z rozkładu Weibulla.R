#Ustawiamy ziarno
set.seed(17)

library('plyr')

#Ustalamy parametr a 
a <- 0.25

#Ustalamy parametry a i b dla rozkładu Weibulla
a_par <- 5

#deterministyczna wielkosc probki

# funkcja pomocnicza wyznaczająca K_{n:n}(a)
generate_weibull <- function(a, N, a_par) {
  y_rweibull <- rweibull(N, a_par, 1)
  y_rweibull_max <- max(y_rweibull)
  K <- count(y_rweibull>y_rweibull_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}



#generowanie dla róznych wielości próbek
generate_weibull(a, 10, a_par)
generate_weibull(a, 100, a_par)
generate_weibull(a, 1000, a_par)
generate_weibull(a, 10000, a_par)
generate_weibull(a, 100000, a_par)
generate_weibull(a, 1000000, a_par)
generate_weibull(a, 10000000, a_par)
generate_weibull(a, 100000000, a_par)
#generate_weibull(a, 1000000000, a_par)


# losowa wielkosc próbki
#N(t) - proces Poissona 
#N(t)  -> \infty

lambda <- 0.5

generate_poisson <- function(lambda, t){
  rhos <- NULL
  i <- 1
  while(sum(rhos) < t){
    samp <- rexp(1, lambda)
    rhos[i] <- samp
    i <- i+1
  }
  return(head(rhos, -1))
}

generate_weibull_poisson <- function(a, t, a_par) {
  poiss <- generate_poisson(lambda, t)
  len <- length(poiss)
  y_rweibull <- rweibull(len, a_par, 1)
  y_rweibull_max <- max(y_rweibull)
  K <- count(y_rweibull>y_rweibull_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}

generate_weibull_poisson(a, 10, a_par) 
generate_weibull_poisson(a, 100, a_par) 
generate_weibull_poisson(a, 1000, a_par) 
generate_weibull_poisson(a, 10000, a_par) 
generate_weibull_poisson(a, 100000, a_par) 
generate_weibull_poisson(a, 1000000, a_par) 
