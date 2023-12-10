#Ustawiamy ziarno
set.seed(17)
library('plyr')

#Ustalamy parametr a 
a <- 1

#Ustalamy parametry a i b dla rozkładu wykladniczego
a_par <- 1

#deterministyczna wielkosc probki

#B(a) = e^{-a}
x_pgeom <- seq(0, 20, by = 0.2)  

sprintf("%.7f",exp(-a))
prob <- exp(-a)

pgeom(x_pgeom, prob = prob)

# funkcja pomocnicza wyznaczająca K_{n:n}(a)
generate_exp <- function(a, N, a_par) {
  y_rexp <- rexp(N, a_par) 
  y_rexp_max <- max(y_rexp)
  K <- count(y_rexp>y_rexp_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}

K <- c()
for (x in 1:3001) {
  K[x] <- generate_exp(a, 100, a_par)
} 

K
length(K)
v <- c()
for (x in x_pgeom ) {
  ile<- count(K<=x)
  v[x*5+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v


#przesunięcie ze względu na różne koncepcje rozkładu geometrycznego
v1 <- c(NA, NA, NA, NA, NA)
pgeo <- c(v1, pgeom(x_pgeom, prob = prob))
pgeo <- pgeo[1:101]

plot(x_pgeom, v, pch="_", col="blue", xlab="x", ylab="F(x)")  
points(x_pgeom, pgeo, pch="_", col="magenta")
legend(13, 0.5, legend=c("Dystrybuanta G ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)


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

generate_exp_poisson <- function(a, t, a_par) {
  poiss <- generate_poisson(lambda, t)
  len <- length(poiss)

  y_rexp <- rexp(len, a_par) 
  y_rexp_max <- max(y_rexp)
  K <- count(y_rexp>y_rexp_max-a)
  K_1 <-K[which(K$x == TRUE),'freq']
  res <- K_1
  return(res)
}


K <- c()
for (x in 1:3001) {
  K[x] <- generate_exp_poisson(a, 100, a_par)
} 

K
length(K)
v <- c()
for (x in x_pgeom ) {
  ile<- count(K<=x)
  v[x*5+1] <- ile[which(ile$x == TRUE),'freq']/length(K)
} 
v


#przesunięcie ze względu na różne koncepcje rozkładu geometrycznego
v1 <- c(NA, NA, NA, NA, NA)
pgeo <- c(v1, pgeom(x_pgeom, prob = prob))
pgeo <- pgeo[1:101]

plot(x_pgeom, v, pch="_", col="blue", xlab="x", ylab="F(x)")  
points(x_pgeom, pgeo, pch="_", col="magenta")
legend(13, 0.5, legend=c("Dystrybuanta G ","Dystrybuanta K "),
       col=c("magenta", "blue"), lty=1:2, cex=0.8)

