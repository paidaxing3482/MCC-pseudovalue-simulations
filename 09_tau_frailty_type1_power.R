library(MCC)


set.seed(123)

B <- 50
alpha <- 0.05

tau <- 10
t_list <- c(2, 3, 4)


lambda_c <- 0.25
lambda_d <- 0.25

frailty_var_list <- c(0, 0.25, 0.5)

N_per_arm <- 200
alphaX <- log(1.25)


type1_result <- expand.grid(
  t_eval = t_list,
  frailty_variance = frailty_var_list
)

type1_result$reject_rate <- NA_real_
type1_result$mean_beta <- NA_real_
type1_result$se_reject <- NA_real_
type1_result$lower <- NA_real_
type1_result$upper <- NA_real_

power_out <- expand.grid(
  t_eval = t_list,
  frailty_variance = frailty_var_list
)

power_out$power <- NA_real_
power_out$mean_beta <- NA_real_
power_out$se_power <- NA_real_
power_out$lower <- NA_real_
power_out$upper <- NA_real_

risk_summary <- expand.grid(
  t_eval = t_list,
  frailty_variance = frailty_var_list
)

risk_summary$risk_prop <- NA_real_

# Type I error
for (j in seq_len(nrow(type1_result))) {
  
  t_eval <- type1_result$t_eval[j]
  frailty_variance <- type1_result$frailty_variance[j]
  
  alphaA <- 0
  
  reject <- logical(B)
  beta <- numeric(B)
  
  for (b in 1:B) {
    if (b == 1) {
      cat("Using frailty setting:",
          "tau =", t_eval,
          "frailty_variance =", frailty_variance,
          "lambda_c =", lambda_c,
          "lambda_d =", lambda_d, "\n")
    }
    
    source("01_generate_data.R")
    
    pseudo_b <- MCC::GenPseudo(data = dat, tau = t_eval)
    
    AX <- unique(dat[, c("idx", "A")])
    reg_dat <- merge(AX, pseudo_b[, c("idx", "pseudo")], by = "idx")
    
    fit <- lm(pseudo ~ A, data = reg_dat)
    s <- summary(fit)$coefficients
    
    beta[b] <- s["A", "Estimate"]
    reject[b] <- s["A", "Pr(>|t|)"] < alpha
  }
  
  type1_result$reject_rate[j] <- mean(reject)
  type1_result$mean_beta[j] <- mean(beta)
  
  type1_result$se_reject[j] <- sqrt(
    type1_result$reject_rate[j] * (1 - type1_result$reject_rate[j]) / B
  )
  
  type1_result$lower[j] <- max(
    0,
    type1_result$reject_rate[j] - 1.96 * type1_result$se_reject[j]
  )
  
  type1_result$upper[j] <- min(
    1,
    type1_result$reject_rate[j] + 1.96 * type1_result$se_reject[j]
  )
  
  cat("Type I done:",
      "tau =", t_eval,
      "frailty_variance =", frailty_variance, "\n")
}

write.csv(type1_result, "type1_tau_frailty_result.csv", row.names = FALSE)

# Power
for (j in seq_len(nrow(power_out))) {
  
  t_eval <- power_out$t_eval[j]
  frailty_variance <- power_out$frailty_variance[j]
  
  alphaA <- log(1.2)
  
  reject <- logical(B)
  beta <- numeric(B)
  risk_prop_vec <- numeric(B)
  
  for (b in 1:B) {
    if (b == 1) {
      cat("Using frailty setting:",
          "tau =", t_eval,
          "frailty_variance =", frailty_variance,
          "lambda_c =", lambda_c,
          "lambda_d =", lambda_d, "\n")
    }
    
    source("01_generate_data.R")
    
    risk_prop_vec[b] <- mean(tapply(dat$time > t_eval, dat$idx, any))
    
    pseudo_b <- MCC::GenPseudo(data = dat, tau = t_eval)
    
    AX <- unique(dat[, c("idx", "A")])
    reg_dat <- merge(AX, pseudo_b[, c("idx", "pseudo")], by = "idx")
    
    fit <- lm(pseudo ~ A, data = reg_dat)
    s <- summary(fit)$coefficients
    
    beta[b] <- s["A", "Estimate"]
    reject[b] <- s["A", "Pr(>|t|)"] < alpha
  }
  
  power_out$power[j] <- mean(reject)
  power_out$mean_beta[j] <- mean(beta)
  
  power_out$se_power[j] <- sqrt(
    power_out$power[j] * (1 - power_out$power[j]) / B
  )
  
  power_out$lower[j] <- max(
    0,
    power_out$power[j] - 1.96 * power_out$se_power[j]
  )
  
  power_out$upper[j] <- min(
    1,
    power_out$power[j] + 1.96 * power_out$se_power[j]
  )
  
  risk_summary$risk_prop[j] <- mean(risk_prop_vec)
  
  cat("Power done:",
      "tau =", t_eval,
      "frailty_variance =", frailty_variance, "\n")
}

write.csv(power_out, "power_tau_frailty_result.csv", row.names = FALSE)
write.csv(risk_summary, "risk_tau_frailty_summary.csv", row.names = FALSE)
save.image("tau_frailty_workspace.RData")