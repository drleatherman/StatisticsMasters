# set seed for reproducability
set.seed(1234)

# initial variables set
s0 <- 50
T <- 1
sigma <- 0.13
n <- 1000 # sample size
d <- 12 # time size
init.r <- 0.07
delta <- T / d
grid <- seq(delta, T, length.out = d) # time grid

# generate nxd pseudo-random normal numbers
z <- matrix(rnorm(n * d), nrow = n)
x <- matrix(rnorm(n * d), nrow = n)
g <- matrix(rgamma(n * d, shape = 0.15, scale = grid / 0.15), nrow = n)


# equation for calculating the vasicek interest rate (Z_j)
vasicek_int_rate <- function(rt_1, delta, VS) {
  rt_1 + 0.18 * (0.086 - rt_1) * delta + VS
}

# equation for caluclation the stock price using the Variance-Gamma formula
# note that "c" is a precomputed constant to speed things up a bit
var_gamma <- function(st_1, delta, VG, rt_1, c) {
  st_1 * exp((rt_1 + c) * delta) * VG
}

# create and populate a matrix that will be used for interest rates (r)
# The first column contains the initial interest rate
# Subsequent columns contain the calculation of the random variables of the Vasicek Interest Rate Formula
# This was done here so it could be precomputed
r <- cbind(rep(init.r, n), 0.02 * sqrt(delta) * t(apply(z, 1, cumsum)))

# Create and populate a matrix to be used for Stock Prices
# Similar to above, calculatinos for the random variables and some transformations have been done to reduce work later on.
# TODO: Does the cumulative sum rule apply to the Variance-Gamma Model? I took it out because I was getting some wild simulations.
s <- cbind(rep(s0, n), exp(sigma * sqrt(g) * x))

# precompute part of the stockprice formula since its just constants
s.c <- log(1 - 0.15 * sigma^2 / 2) / 0.15

# simulate the Jackpot Algorithm
simVasicekVarGamma <- function(d, r, s) {
  # Each column is a time point "d". Each row is a simulation.
  # Loop over each column and calculate the stock price for each "d".
  # The loop is done because theres a need to maintain order in calculations.
  # Thus, using an index is necessary. Since each column must be processed in order, I doubt that this can be parallelized using the apply family due to that restriction.
  # start at 2 since index 1 contains the starting value
  for (i in seq(from = 2, (d + 1))) {
    r.i <- r[, i]
    r.prev <- r[, i - 1]

    s.i <- s[, i]
    s.prev <- s[, i - 1]

    rt <- vasicek_int_rate(r.prev, grid[i - 1], r.i)
    r[, i] <- rt

    st <- var_gamma(s.prev, grid[i - 1], s.i, r.prev, s.c)
    s[, i] <- st
  }
  # return stock price and interest if interested in investigating
  list(s = s, r = r)
}

sampleSim <- simVasicekVarGamma(d, r, s)

payoff <- pmax(sampleSim$s[, ncol(s)] - s0, 0) * exp(-init.r * T)

est.price <- mean(payoff)

init.payoff.sigma <- sd(payoff)

est.n <- ceiling((2.58 * 1.1 * init.payoff.sigma / 0.05)^2)

# Now that we know the full N needed to calculate up to the desired accuracy. Do it all over again
z.full <- matrix(rnorm(est.n * d), nrow = est.n)
x.full <- matrix(rnorm(est.n * d), nrow = est.n)
g.full <- matrix(rgamma(est.n * d, shape = 0.15, scale = grid / 0.15), nrow = est.n)

r.full <- cbind(rep(init.r, est.n), 0.02 * sqrt(delta) * t(apply(z.full, 1, cumsum)))
# does the cumulative sum rule apply to the Variance-Gamma Model?
s.full <- cbind(rep(s0, est.n), exp(sigma * sqrt(g.full) * x.full))

sim <- simVasicekVarGamma(d, r.full, s.full)

payoff.full <- pmax(sim$s[, ncol(s.full)] - s0, 0) * exp(-init.r * T)

est.price <- mean(payoff.full)

lcl <- est.price - 1.96 * sd(payoff.full) / sqrt(est.n)
ucl <- est.price + 1.96 * sd(payoff.full) / sqrt(est.n)
