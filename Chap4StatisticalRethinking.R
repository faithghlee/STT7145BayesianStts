#CODE ALONG FROM STATISTICAL RETHINKING 
library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]
mu.list <- seq( from=150, to=160 , length.out=100 ) 

sigma.list <- seq( from=7 , to=9 , length.out=100 )
post <- expand.grid( mu=mu.list , sigma=sigma.list ) #ach combination of mu and sigma 

#\sum_{i=1} density of all 352 datapoints for each combination of mu and sigma. - log scale 
post$LL <- sapply( 1:nrow(post) , function(i) sum(
  dnorm( d2$height , post$mu[i] , post$sigma[i] , log=TRUE ) ) )

#\pi(\theta|x) \propto likelihood(x|theta)p(\theta) 
#the prior for the mu is Normal distribution with mu 178, sigma 20 
#the prior for sigma is uniform of 0, 50
#proportional posterior distribution 
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE ) 

#Scaling as mentioned in the book: also the f(x)dx is a probability. so f(x) is a probability up to some constant
post$prob <- exp( post$prod - max(post$prod) ) 
