# 01_generate_data.R


# push test from RStudio


# Generate data according to the simulation plan (DGP) using MCC
#rm(list = ls())

library(MCC)


# 1) Set main parameters

tau <- 10
delta <- 0.2

lambda_from_delta <- function(delta, tau) {
  -(1 / tau) * log(1 - delta)
}

lambda_c <- lambda_from_delta(delta, tau)
lambda_d <- lambda_from_delta(delta, tau) 

m <- 4
base_event_rate <- m / tau

alphaA <- 0
alphaX <- log(1.25)

N_per_arm <- 50
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
  frailty_variance = 0,
  min_death_rate   = 1e-6,
  min_event_rate   = 1e-6,
  n               = n_total,
  tau             = tau
)


# 4) Sanity checks

table(dat$status)

events_per_id <- tapply(dat$status == 1, dat$idx, sum)
summary(events_per_id)

death_per_id <- tapply(dat$status == 2, dat$idx, any)

mean(death_per_id)

mean(death_per_id)

