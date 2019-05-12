
# Chpt 4 Linear Models

library(rethinking)

# 4.1.1. Normalbyaddition. 
# Let’sseethisresult,bysimulatingthisexperimentinR.
# Toshow that there’s nothing special about the underlying coin flip, assume instead that each step is different from all the others, 
# a random distance between zero and one yard. Thus a coin is flipped, a distance between zero and one yard is taken in the indicated direction, and the process repeats. To simulate this, we generate for each person a list of 16 random numbers between −1 and 1. These are the individual steps. Then we add these steps together to get the position after 16 steps. Then we need to replicate this procedure 1000 times. This is the sort of task that would be harrowing in a point-and-click interface, but it is made trivial by the command line. Here’s a single line to do the whole thing:
runif(16,-1,1)
pos <- replicate( 1000 , sum( runif(16,-1,1) ) )
                  
head(pos)

hist(pos)
plot(density(pos))

x <- seq(0,2*pi,0.1)
y <- sin(x)

# R code 4.2
prod( 1 + runif(12,0,0.1) )

# R code 4.2
growth <- replicate( 10000 , prod( 1 + runif(12,0,0.1) ) )
plot(density( growth , norm.comp=TRUE ))

prod( 1 + runif(12,0,0.1) )
growth
1 + runif(12,0,0.1)

dens( growth , norm.comp=TRUE )

big <- replicate( 10000 , prod( 1 + runif(12,0,0.5) ) )
small <- replicate( 10000 , prod( 1 + runif(12,0,0.01) ) )

dens( big , norm.comp=TRUE )
dens( small , norm.comp=TRUE )

log.big <- replicate( 10000 , log(prod(1 + runif(12,0,0.5))) )
dens( log.big , norm.comp=TRUE )

data_set1 = c(3, 4, 4, 5, 6, 8)
data_set2 = c(1, 2, 4, 5, 7, 11)

mean(data_set1)
mean(data_set2)

var(data_set1)
var(data_set2)

sd(data_set1)
sd(data_set2)

library(rethinking)
data(Howell1)
d <- Howell1

str( d )

d$height
d[[1]]
d[['height']]

d2 <- d[ d$age >= 18 , ]


#R code 4.12
curve( dnorm( x , 178 , 20 ) , from=100 , to=250 )

curve( dunif( x , 0 , 50 ) , from=-10 , to=60 )

sample_mu <- rnorm( 1e4 , 178 , 20 )
sample_sigma <- runif( 1e4 , 0 , 50 )
prior_h <- rnorm( 1e4 , sample_mu , sample_sigma )
dens( prior_h )

mu.list <- seq( from=140, to=160 , length.out=200 )
sigma.list <- seq( from=4 , to=9 , length.out=200 )
post <- expand.grid( mu=mu.list , sigma=sigma.list )
post$LL <- sapply( 1:nrow(post) , function(i) sum( dnorm(
  d2$height ,
  mean=post$mu[i] ,
  sd=post$sigma[i] ,
  log=TRUE ) ) )
post$prod <- post$LL + dnorm( post$mu , 178 , 20 , TRUE ) +
  dunif( post$sigma , 0 , 50 , TRUE )
post$prob <- exp( post$prod - max(post$prod) )

contour_xyz( post$mu , post$sigma , post$prob )

image_xyz( post$mu , post$sigma , post$prob )

sample.rows <- sample( 1:nrow(post) , size=1e4 , replace=TRUE ,
                       prob=post$prob )
sample.mu <- post$mu[ sample.rows ]
sample.sigma <- post$sigma[ sample.rows ]


library(rethinking)
data(Howell1)
d <- Howell1
d2 <- d[ d$age >= 18 , ]

flist <- alist(
  height ~ dnorm( mu , sigma ) ,
  mu ~ dnorm( 178 , 20 ) ,
  sigma ~ dunif( 0 , 50 )
)

m4.1 <- map( flist , data=d2 )

precis( m4.1 )

var = c(0.16973959, 0.08490579)
sqrt(var)

HPDI( sample.mu )
HPDI( sample.sigma )

0.1*0.1

4^2

1/20^2

exp(2)

log(exp(3))

# rethinking problem 1



