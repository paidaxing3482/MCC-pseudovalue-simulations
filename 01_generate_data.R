# 01_generate_data.R

library(MCC)

# 1) Set main parameters

if (!exists("tau")) tau <- 10
if (!exists("lambda_c")) lambda_c <- 0.25
if (!exists("lambda_d")) lambda_d <- 0.25
if (!exists("frailty_variance")) frailty_variance <- 0

m <- 4
base_event_rate <- m / tau

if (!exists("alphaA")) alphaA <- 0
if (!exists("alphaX")) alphaX <- log(1.25)

# Allow external scripts to control sample size safely
if (exists("N_per_arm_external")) {
  N_per_arm <- N_per_arm_external
} else {
  if (!exists("N_per_arm")) N_per_arm <- 200
}

n_total <- 2 * N_per_arm


# 2) Generate covariates
A <- rep(c(0, 1), each = N_per_arm)
X <- rnorm(n_total)
covariates <- cbind(A = A, X = X)

beta_event <- c(alphaA, alphaX)
beta_death <- c(0, 0)


# 3) Generate data
dat <- GenData(
  base_death_rate  = lambda_d,
  base_event_rate  = base_event_rate,
  beta_death       = beta_death,
  beta_event       = beta_event,
  censoring_rate   = lambda_c,
  covariates       = covariates,
  frailty_variance = frailty_variance,
  min_death_rate   = 1e-6,
  min_event_rate   = 1e-6,
  n                = n_total,
  tau              = tau
)


# 4) Sanity checks
table(dat$status)

events_per_id <- tapply(dat$status == 1, dat$idx, sum)
summary(events_per_id)

death_per_id <- tapply(dat$status == 2, dat$idx, any)
mean(death_per_id)
