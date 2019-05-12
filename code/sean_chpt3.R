
library(rethinking)

# rethinking problem 1
PrPT = 0.95
PrPN = 0.05
PrT = 0.01
PrN = 1 - PrT

PrTP = (PrPT * PrT) / ((PrPT * PrT) + (PrPN * PrN))


p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
sum(posterior)
posterior <- posterior / sum(posterior)

p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 1 , size=9 , prob=p_grid )
posterior <- likelihood * prior
sum(posterior)
posterior <- posterior / sum(posterior)
head(likelihood)
plot(posterior)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
plot(samples)
dens(samples)

dbinom( 6 , size=9 , prob=0.7 )
dbinom(0, size=12, prob=0.2)

dbinom(1, size=12, prob=0.2) + 
dbinom(2, size=12, prob=0.2) + 
dbinom(3, size=12, prob=0.2) + 
dbinom(4, size=12, prob=0.2) 

dbinom(12, size=12, prob=0.2)

sum(posterior[ p_grid < 0.5 ])

posterior[3]
head(posterior)
p_grid < 0.5
1e4

sum( samples < 0.5) / 1e4
sum( samples > 0.5 & samples < 0.75) / 1e4

c(0:2, 0:3)

dummy_w = rbinom(1e5, size=2, prob = 0.7)
head(dummy_w)
table(dummy_w)
table(dummy_w)/1e5
sum(table(dummy_w)/1e5)

p_grid <- seq( from=0 , to=1 , length.out=10 )
prior <- rep( 1 , 10 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)

samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )
head(samples)
library(rethinking)
dens( samples )

# R code 3.3


# 3.2.1. Intervals of defined boundaries. 

