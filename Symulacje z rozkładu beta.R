#Ustawiamy ziarno
set.seed(17)

library('plyr')

#Ustalamy parametr a 
a <- 0.5

x <- 1-a
x

#Ustalamy parametry a i b dla rozkładu beta
a_beta <- 1
b_beta <- 5

pr_beta <- 1-pbeta(x, shape1 = a_beta, shape2 = b_beta)
pr_beta
sprintf("%.7f",pr_beta)

#deterministyczna wielkosc probki

# funkcja pomocnicza wyznaczająca K_{n:n}(a) / n
generate_beta <- function(a, N, a_beta, b_beta) {
  y_rbeta <- rbeta(N, shape1 = a_beta, shape2 = b_beta) 
  y_rbeta_max <- max(y_rbeta)
  K <- count(y_rbeta>y_rbeta_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1/N
  return(res)
}

#generowanie dla róznych wielości próbek
sprintf("%.7f",pr_beta)
generate_beta(a, 10, a_beta,b_beta)
generate_beta(a, 100, a_beta,b_beta)
generate_beta(a, 1000, a_beta,b_beta)
generate_beta(a, 10000, a_beta,b_beta)
generate_beta(a, 100000, a_beta,b_beta)
generate_beta(a, 1000000, a_beta,b_beta)
generate_beta(a, 10000000, a_beta,b_beta)
sprintf("%.7f",generate_beta(a, 100000000, a_beta,b_beta))

# losowa wielkosc próbki
#N(t) - proces Poissona 
#N(t) / t -> lambda

lambda <- 2

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

generate_beta_poisson <- function(a, t, a_beta, b_beta) {
  poiss <- generate_poisson(lambda, t)
  len <- length(poiss)
  y_rbeta <- rbeta(len, shape1 = a_beta, shape2 = b_beta) 
  y_rbeta_max <- max(y_rbeta)
  K <- count(y_rbeta>y_rbeta_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1/t
  return(res)
}

sprintf("%.7f",lambda*pr_beta) 
generate_beta_poisson(a, 10, a_beta,b_beta) 
generate_beta_poisson(a, 100, a_beta,b_beta) 
generate_beta_poisson(a, 1000, a_beta,b_beta) 
generate_beta_poisson(a, 10000, a_beta,b_beta) 
generate_beta_poisson(a, 100000, a_beta,b_beta) 
generate_beta_poisson(a, 1000000, a_beta,b_beta) 
