library(MCC)

set.seed(123)

scenarios <- data.frame(
  scenario = c("baseline", "high_censor", "high_death", "high_both"),
  lambda_c = c(0.25, 0.40, 0.25, 0.40),
  lambda_d = c(0.25, 0.25, 0.40, 0.40)
)

B <- 1000
alpha <- 0.05
tau_list <- c(2, 2.5)

run_type1 <- function(t_eval) {
  
  alphaA <- 0
  
  type1_result <- data.frame(
    tau = t_eval,
    scenario = scenarios$scenario,
    lambda_c = scenarios$lambda_c,
    lambda_d = scenarios$lambda_d,
    reject_rate = NA_real_,
    mean_beta = NA_real_,
    se_reject = NA_real_,
    lower = NA_real_,
    upper = NA_real_
  )
  
  for (j in seq_len(nrow(scenarios))) {
    
    lambda_c <- scenarios$lambda_c[j]
    lambda_d <- scenarios$lambda_d[j]
    
    reject <- logical(B)
    beta <- numeric(B)
    
    for (b in 1:B) {
      
      source("01_generate_data.R", local = TRUE)
      
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
  }
  
  return(type1_result)
}

run_power <- function(t_eval) {
  
  alphaA <- log(1.2)
  
  power_out <- data.frame(
    tau = t_eval,
    scenario = scenarios$scenario,
    lambda_c = scenarios$lambda_c,
    lambda_d = scenarios$lambda_d,
    power = NA_real_,
    mean_beta = NA_real_,
    se_power = NA_real_,
    lower = NA_real_,
    upper = NA_real_
  )
  
  risk_summary <- data.frame(
    tau = t_eval,
    scenario = scenarios$scenario,
    lambda_c = scenarios$lambda_c,
    lambda_d = scenarios$lambda_d,
    risk_prop = NA_real_
  )
  
  for (j in seq_len(nrow(scenarios))) {
    
    lambda_c <- scenarios$lambda_c[j]
    lambda_d <- scenarios$lambda_d[j]
    
    reject <- logical(B)
    beta <- numeric(B)
    
    for (b in 1:B) {
      
      source("01_generate_data.R", local = TRUE)
      
      terminal_dat <- dat[dat$status != 1, ]
      cat("USING TERMINAL_DAT VERSION\n")
      risk_prop <- mean(terminal_dat$time > t_eval)
      
      if (b == 1) {
        risk_summary$risk_prop[j] <- risk_prop
      }
      
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
  }
  
  return(list(power_out = power_out, risk_summary = risk_summary))
}

all_type1 <- list()
all_power <- list()
all_risk <- list()

for (tt in tau_list) {
  
  cat("Running tau =", tt, "\n")
  
  type1_result <- run_type1(t_eval = tt)
  power_res <- run_power(t_eval = tt)
  
  all_type1[[as.character(tt)]] <- type1_result
  all_power[[as.character(tt)]] <- power_res$power_out
  all_risk[[as.character(tt)]] <- power_res$risk_summary
}

type1_all <- do.call(rbind, all_type1)
power_all <- do.call(rbind, all_power)
risk_all  <- do.call(rbind, all_risk)

print(type1_all)
print(power_all)
print(risk_all)

write.csv(type1_all, "type1_compare_tau_terminal.csv", row.names = FALSE)
write.csv(power_all, "power_compare_tau_terminal.csv", row.names = FALSE)
write.csv(risk_all,  "risk_compare_tau_terminal.csv", row.names = FALSE)