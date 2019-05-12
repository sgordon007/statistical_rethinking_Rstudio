# https://github.com/rmcelreath/rethinking
# https://github.com/fxcoudert/gfortran-for-macOS/releases
# https://cran.r-project.org/
# https://cran.r-project.org/web/packages/mvtnorm/index.html
# https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

library(rethinking)

x <- 1:2
x <- x*10
x <- log(x)
x <- sum(x)
x <- exp(x)
x

# Load the data:
# car braking distances in feet paired with speeds in km/h
# see ?cars for details
data(cars)
# fit a linear regression of distance on speed
m <- lm( dist ~ speed , data=cars )
# estimated coefficients from the model
coef(m)
# plot residuals against speed
plot( resid(m) ~ speed , data=cars )

x = dbinom( 6 , size=9 , prob=0.5 )

q = c(25,75)
hist(rbinom(200, 2000, 0.5))

?dbinom

## Using "log = TRUE" for an extended range :
n <- 2000
k <- seq(0, n, by = 20)
plot (k, dbinom(k, n, pi/10, log = TRUE), type = "l", ylab = "log density",
      main = "dbinom(*, log=TRUE) is better than  log(dbinom(*))")
lines(k, log(dbinom(k, n, pi/10)), col = "red", lwd = 2)
## extreme points are omitted since dbinom gives 0.
mtext("dbinom(k, log=TRUE)", adj = 0)
mtext("extended range", adj = 0, line = -1, font = 4)
mtext("log(dbinom(k))", col = "red", adj = 1)

# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

library(rethinking)
globe.qa <- map(
  alist(
    w ~ dbinom(9,p) ,  # binomial likelihood
    p ~ dunif(0,1)     # uniform prior
  ), data=list(w=6) )
# display summary of quadratic approximation
precis( globe.qa )

# Homework

#2M1
# define grid
p_grid <- seq( from=0 , to=1 , length.out=20 )
# define prior
prior <- rep( 1 , 20 )
# compute likelihood at each value in grid
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
# compute product of likelihood and prior
unstd.posterior <- likelihood * prior
# standardize the posterior, so it sums to 1
posterior <- unstd.posterior / sum(unstd.posterior)

plot( posterior ~ p_grid , type="l" )

# 2M2
p_grid <- seq( from=0 , to=1 , length.out=100 )
likelihood <- dbinom( 3 , size=3 , prob=p_grid )
prior <- ifelse( p_grid < 0.5 , 0 , 1 ) # new prior
posterior <- likelihood * prior
posterior <- posterior / sum(posterior) # standardize
plot( posterior ~ p_grid , type="l" )

dbinom( 6 , size=9 , prob=0.667 )
6/9


