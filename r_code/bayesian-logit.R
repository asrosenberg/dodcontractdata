## =============================================================================
## Bayesian Logistic Regression Estimated with the Metropolis Algorithm
## By: Jason W. Morgan
## =============================================================================

library(lattice)

## Fake data example
set.seed(95658)
n <- 1000
X <- cbind(1, rnorm(n))
b <- c(-1.0, 2.5)
y <- rbinom(n, 1, plogis(X %*% b))

iter <- 10000
burnin <- 5000
S <- run_MH(c(0, 0), y, X, iter=iter)
S <- as.data.frame(S)
colnames(S) <- c("b0", "b1")

Keep <- as.data.frame(S[(burnin+1):iter,])

histogram(~ b0, data=Keep)
histogram(~ b1, data=Keep)

xyplot(b0 ~ 1:nrow(Keep), data=Keep, type="l")
xyplot(b0 ~ 1:nrow(S), data=S, type="l")

xyplot(b1 ~ 1:nrow(Keep), data=Keep, type="l")
xyplot(b1 ~ 1:nrow(S), data=S, type="l")

xyplot(b0 ~ b1, data=S, type="l")
xyplot(b0 ~ b1, data=Keep, type="l")

## Compare posterior mean to standard GLM estimates
rbind(Bayes=colMeans(Keep),
      GLM=coef(glm(y ~ X[,2], family=binomial)))


## -----------------------------------------------------------------------------
## Code
## -----------------------------------------------------------------------------

logit_llik <- function(beta, y, X)
{
    lambda <- plogis(X %*% beta)
    sum(dbinom(y, 1, lambda, log=TRUE))
}

log_prior <- function(beta, sd=5)
{
    b0 <- dnorm(beta[1], sd=sd, log=TRUE)
    b1 <- dnorm(beta[2], sd=sd, log=TRUE)
    b0 + b1
}

log_posterior <- function(beta, y, X)
{
    logit_llik(beta, y, X) + log_prior(beta)
}

proposal <- function(beta)
{
    rnorm(length(beta), mean=beta, sd=0.1)
}

MH_step <- function(beta, y, X)
{
    proposed_beta <- proposal(beta)

    p <- exp(log_posterior(proposed_beta, y, X) - log_posterior(beta, y, X))
    
    if (runif(1) < p)
        proposed_beta
    else
        beta
}

run_MH <- function(start, y, X, iter=10)
{
    samples <- matrix(0, ncol=length(start), nrow=iter)
    samples[1,] <- current <- start
    for (i in 2:iter) {
        current <- samples[i,] <- MH_step(current, y, X)
    }

    samples
}
