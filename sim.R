library(ineq)

set.seed(1234)

## Suppose there are 100 projects in the first period, and this goes up to 200 projects in the second period. 
N <- 100

## Project "quality" is drawn from the same uniform distribution in both periods.
## Here, qualities are ordered from best to worst:

q.0 <- sort(runif(N), decreasing = TRUE)
q.1 <- sort(runif(2*N), decreasing = TRUE)

## Suppose the market is efficient and backers have to pay q for a project of quality q. 
## And suppose all backers have a budget of B.
## They invest in the best projects until they hit their budget constraint.  

## Let's say the budget is 25, and so not all projects can be funded. 
B <- 25

## We can find the "marginal" project i.e., the worse one still funded by taking the cumulative sum
## and seeing where we hit the budget constraint:

cum.q.0 <- cumsum(q.0)
cum.q.1 <- cumsum(q.1)
num.funded.0 <- length(cum.q.0[cum.q.0 < B])
num.funded.1 <- length(cum.q.0[cum.q.1 < B])

## We can then create the distribution of funds received in both periods:

funds.0 <- q.0
funds.0[num.funded.0:N] <- 0

funds.1 <- q.1
funds.1[num.funded.1:(2*N)] <- 0

## Then we can compute the Gini coefficient for both: 

ineq(funds.0,type="Gini")
ineq(funds.1,type="Gini")

## We can se there is an increase in the Gini index in the period when there are more projects to choose from:

## > ineq(funds.0,type="Gini")
## [1] 0.7142446
## > ineq(funds.1,type="Gini")
## [1] 0.877661

## Also, that funded projects get more money, conditional upon getting any:

mean(funds.0[funds.0 > 0])
mean(funds.1[funds.1 > 0])

## > mean(funds.0[funds.0 > 0])
## [1] 0.7835555
## > mean(funds.1[funds.1 > 0])
## [1] 0.9522472


