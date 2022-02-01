#Section 5 of Ford's Notes to understand two-dimension Metropolis Hasting 

#prior parameter specification 
a <- 10 
b <- 10 

flips_1 <- 25; heads_1 <- 17 
flips_2 <-9; heads_2 <- 1

thetas<- seq(0, 1, length.out= 50)
prior_1 <- dbeta(thetas, shape1 = a, shape2 = b)
prior_2 <- dbeta(thetas, shape1 = a, shape2 = b)

likelihood_1<- dbinom(heads_1, size = flips_1, prob = thetas)
likelihood_2 <- dbinom(heads_2, size = flips_2, prob = thetas)

posterior_1 <- dbeta(thetas, shape1 = (a+heads_1), shape2=b + flips_1-heads_1)
posterior_2 <- dbeta(thetas, shape1 = (a+heads_2), shape2=b + flips_2-heads_2)

joint_prior <- outer(prior_1, prior_2, FUN='*' )
joint_likelihood <- outer(likelihood_1, likelihood_2, FUN='*')
joint_posterior <- outer(posterior_1, posterior_2, FUN = '*')

### TO USE a 2D Metropolis Hasting 

# Previously theta was normally distributed. Now theta is a vector of two values that will be binvariate normal distributed 
#the covariance matrix will be 0 on the diagonals 

library(MASS)

flips <- c(flips_1, flips_2)
heads <- c(heads_1, heads_2)

metropolis_algorithm_2d<- function(samples, thetas_seed, cov_mat){
  
  thetas_curr <- thetas_seed 
  
  posterior_thetas <- matrix(NA, nrow = samples, ncol=2)
  
  for (i in 1:samples){
    thetas_prop <- mvrnorm(n=1, mu=thetas_curr, Sigma=cov_mat) #1 by2 vector 
    thetas_prop <- mapply(FUN = function(x, y) ifelse((x > 1 | x < 0), y, x),
                          x = thetas_prop,
                          y = thetas_curr)
    
    posterior_prop <- dbeta(thetas_prop, shape1 = a, shape2=b)*dbinom(heads, size = flips, prob=thetas_prop)
    posterior_curr <- dbeta(thetas_curr, shape1 = a, shape2=b)*dbinom(heads, size = flips, prob=thetas_curr)
    
    #pmin takes one or more vectors and return a single vector with parallel minima 
    p_accept_theta_prop <- pmin(posterior_prop/posterior_curr, 1.0)
    
    rand_unif <- runif(n=2)
    
    #If probability of accept is greater than the random uniform, 
    
    thetas_select <- mapply(FUN = function(x, y, w, z) ifelse(x > y, w, z),
                            x = p_accept_theta_prop,
                            y = rand_unif,
                            w = thetas_prop,
                            z = thetas_curr)
    
    #basically evaluate element by element... 
    
    posterior_thetas[i,] <- thetas_select
    #Reset theta
    thetas_curr <-thetas_select
    
  }
  return(posterior_thetas)
}


posterior_thetas_2d <- metropolis_algorithm_2d(20000, thetas_seed = c(0.5, 0.5)
                                               ,cov_mat = matrix(c(0.05^2, 0, 0, 0.05^2)
                                                                 ,ncol=2))


### LEARNING POINTS ## 
#DONT HAVE TO VECTORIZE THE OPERATIONS but it is more convenient for posterior_prop calcs. 
#the posterior mode is concentrated at theta_1 = 0.5; the prior mode is concentrated at the 0.60, the 
warmup <- seq(from = 1, to = 5000) # 1,2,...,5000
posterior_thetas_2d_post_warmup <- posterior_thetas_2d[-warmup, ] # Remove warmup period
n <- nrow(posterior_thetas_2d_post_warmup) # Number of samples to draw from the exact

d <- density(posterior_thetas_2d_post_warmup[,1])
plot( d
      ,main = expression(paste('Kernel Density Plot for ', theta_1))
      ,xlab = expression(theta_1)
      ,ylab = 'density'
      ,yaxt = 'n'
      ,cex.lab = 1.3
)
polygon(d, col='dodgerblue1', border='dark blue')
### OVERLAY THE PLOT WITH THE ACTUAL POSTERIOR DISTRIBUTION WHICH WE KNOW IS BETA(15, 30)
d2<-rbeta(n, shape1=a+heads_1, shape2= b+flips_1-heads_1) 
lines(density(d2), col="red", lwd=5)
quantile(d2, c(0.025,0.05, 0.25, 0.5
           ,0.75, 0.95, 0.975))
