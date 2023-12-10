#Ustawiamy ziarno
set.seed(17)


install.packages('EnvStats')
library('plyr')

#Ustalamy parametr a 
a <- 10

#Ustalamy parametry a i b dla rozkładu Pareto
a_par <- 1

#deterministyczna wielkosc probki

# funkcja pomocnicza wyznaczająca K_{n:n}(a)
generate_pareto <- function(a, N, a_par) {
  y_rpareto <- rpareto(N, a_par, 1) 
  y_rpareto_max <- max(y_rpareto)
  K <- count(y_rpareto>y_rpareto_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}

#generowanie dla róznych wielości próbek
generate_pareto(a, 10, a_par)
generate_pareto(a, 100, a_par)
generate_pareto(a, 1000, a_par)
generate_pareto(a, 10000, a_par)
generate_pareto(a, 100000, a_par)
generate_pareto(a, 1000000, a_par)
generate_pareto(a, 10000000, a_par)


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

generate_pareto_poisson <- function(a, t, a_par) {
  poiss <- generate_poisson(lambda, t)
  len <- length(poiss)
  y_rpareto <- rpareto(len, a_par, 1) 
  y_rpareto_max <- max(y_rpareto)
  K <- count(y_rpareto>y_rpareto_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}

generate_pareto_poisson(a, 10, a_par) 
generate_pareto_poisson(a, 100, a_par) 
generate_pareto_poisson(a, 1000, a_par) 
generate_pareto_poisson(a, 10000, a_par) 
generate_pareto_poisson(a, 100000, a_par) 
generate_pareto_poisson(a, 1000000, a_par) 
