# The theta is proposed from a normal distribution 
# The coin flips are binomial 
# the prior is beta 

#Observed data
flips <- 41
heads <- 13 
#initial observed data; prior distribution 
a<- 2 
b<- 2


metropolis_algorithm <- function(samples, theta_seed, sd){
  #samples - number of samples we want to draw from the posterior distribution 
  #theta_seed is the initial theta 
  #sd- standard deviation of the proposal distribution which is normal distribution 
  theta_curr <- theta_seed #theta is a probability between 0 and 1
  #create vector of NA's to store sampled parameters
  posterior_thetas <- rep(NA, times = samples)
  for (i in 1:samples){
    theta_prop <- rnorm(n=1, mean = theta_curr, sd=sd) #proposal distribution 
    
    #Compute the bayes numerators 
    
    #if the proposed value is outside the range 
    theta_prop <- ifelse((theta_prop<0 | theta_prop>1), theta_curr, theta_prop)
    
    #Bayes' numerators 
    #likelihood given prior x p(prior)
    posterior_prop<- dbeta(theta_prop, shape1=a, shape2=b)*dbinom(heads, size = flips, prob=theta_prop)
    posterior_curr<- dbeta(theta_curr, shape1=a, shape2=b)*dbinom(heads, size = flips, prob=theta_curr)
    
    #calculate the probability of accepting 
    p_accept_theta_prop <- min(posterior_prop/posterior_curr, 1.0 )
    rand_unif <- runif(n=1)
    
    theta_select<- ifelse(p_accept_theta_prop>rand_unif, theta_prop, theta_curr)
    posterior_thetas[i] <- theta_select
    theta_curr <- theta_select
  }
  return(posterior_thetas)
}

set.seed(555)
posterior_thetas <- metropolis_algorithm(samples = 10000, theta_seed = 0.9, sd = 0.05)

opar <- par()
par(mar=c(2.5,3.5,3,2.1), mgp = c(1.7, 1, 0))
d <- density(posterior_thetas)
plot( d
      ,main = expression(paste('Kernel Density Plot for ', theta))
      ,xlab = expression(theta)
      ,ylab = 'density'
      ,yaxt = 'n'
      ,cex.lab = 1.3
)
polygon(d, col='dodgerblue1', border='dark blue')
### OVERLAY THE PLOT WITH THE ACTUAL POSTERIOR DISTRIBUTION WHICH WE KNOW IS BETA(15, 30)
y<- seq(0,1, by = 0.01)
d2<-dbeta(y, shape1=15, shape2= 30) 
d3<-density(rbeta(1000, shape=15, shape2=30))
lines(d3, lwd = 10, col="red")


