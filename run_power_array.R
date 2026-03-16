library(MCC)

set.seed(123)

B <- 1000
alpha <- 0.05
t_list <- c(2.5, 5, 7.5)

alphaA_list <- c(log(1.2), log(1.3), log(1.4))
label_list  <- c("1p2", "1p3", "1p4")

task_id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
alphaA <- alphaA_list[task_id]
effect_label <- label_list[task_id]

power_out <- data.frame(
  effect_label = effect_label,
  alphaA = alphaA,
  t_eval = t_list,
  power = NA_real_,
  mean_beta = NA_real_,
  se_power = NA_real_,
  lower = NA_real_,
  upper = NA_real_
)

for (j in seq_along(t_list)) {
  
  t_eval <- t_list[j]
  reject <- logical(B)
  beta <- numeric(B)
  
  for (b in 1:B) {
    source("01_generate_data.R")
    
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
  power_out$se_power[j] <- sqrt(power_out$power[j] * (1 - power_out$power[j]) / B)
  power_out$lower[j] <- max(0, power_out$power[j] - 1.96 * power_out$se_power[j])
  power_out$upper[j] <- min(1, power_out$power[j] + 1.96 * power_out$se_power[j])
}

outfile <- paste0("power_out_", effect_label, ".csv")
write.csv(power_out, outfile, row.names = FALSE)


