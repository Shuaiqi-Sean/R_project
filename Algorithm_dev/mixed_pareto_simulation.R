# In Philbrick's paper it appears that only a single censor exists in the data. In our case, since CAW losses are capped at the policy limit
# we encounter many censors. This analysis validates the production approach of "mixing" censors together. That is, calculating the
# pareto parameter 'q' with all CSLs present in the data set (as opposed to perhaps handling them individually).

# First, define the censored single-parameter pareto distribution functions.
p.pdf  <- function(q, x) { q*x^(-q-1) }
p.cdf  <- function(q, x) { 1 - x^(-q) }
p.surv <- function(q, x) { x^-q } # 1 - p.cdf
p.rand <- function(q, n) { exp(-log(1-runif(n))/q) }

# We will compare the production approach against a ground-up solution. Here we will define a likelihood function that can handle
# any number of censors. This is accomplished by having two conditions:
#   likelihood(x) =    pdf(x)     x < censor    (probability of being exactly x)
#                      1-cdf(x)   x == censor   (probability of being >= x)
# Clearly, the probability returned by this function will be an appropriate representation regardless of what the censor is,
# and thus maximizing this function is the optimal solution for choosing 'q'.
p.likelihood <- function(q, X, censored=rep(F, length(X))) {
  lt_c <- X[!censored]
  ge_c <- X[censored]
  sum(log(p.pdf(q, lt_c))) + sum(log(p.surv(q, ge_c)))
}

# Optimizer.
p.mle <- function(X, ...) {
  obj <- function(q) { -p.likelihood(q, X, ...) }
  optim(1.00, obj, method="BFGS")
}

# Generate simulated losses with two censors (20 & 30).
losses_normalized <- p.rand(1.05, 100000)
losses_max <- sapply(1:length(losses_normalized), function(i) ifelse(runif(1)>0.5, 20, 30))
losses_capped <- ifelse(losses_normalized >= losses_max, losses_max, losses_normalized)
losses_censored <- losses_capped == losses_max
hist(losses_capped, main="Example - Simulated Losses (q=1.05)")

# Building intution - plot out various choices of 'q' and the corresponding likelihood function.
rng <- seq(1.00, 1.10, by=0.005)
qs <- sapply(rng, function(q) p.likelihood(q, losses_capped, losses_censored))
plot(rng, qs, type='l', main='Likelihood Function', xlab='q', ylab='Log Likelihood')

# Solve for q.
res <- p.mle(losses_capped, censored=losses_censored)
print(res)

# Compare against production process.
prod <- (length(losses_capped) - sum(losses_censored)) / sum(log(losses_capped))
print(prod)

print(res$par - prod)

# Solutions converge to approximately the same parameter.
