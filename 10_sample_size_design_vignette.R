library(MCC)


set.seed(123)

B <- 50
alpha <- 0.05


tau <- 10
t_eval_fixed <- 4


lambda_c_fixed <- 0.25
lambda_d_fixed <- 0.25
frailty_variance_fixed <- 0

alphaX <- log(1.25)


effect_size_list <- c(log(1.2), log(1.3), log(1.4))
n_per_arm_list <- c(50, 100, 150, 200, 250, 300)

design_power <- expand.grid(
  N_per_arm = n_per_arm_list,
  alphaA = effect_size_list
)

design_power$power_adjusted <- NA_real_
design_power$power_treatment_only <- NA_real_

for (j in seq_len(nrow(design_power))) {
  
  N_per_arm_current <- design_power$N_per_arm[j]
  alphaA_current <- design_power$alphaA[j]
  
  reject_adjusted <- logical(B)
  reject_treatment <- logical(B)
  
  for (b in 1:B) {
    N_per_arm <- N_per_arm_current
    alphaA <- alphaA_current
    t_eval <- t_eval_fixed
    lambda_c <- lambda_c_fixed
    lambda_d <- lambda_d_fixed
    frailty_variance <- frailty_variance_fixed
    
    if (b == 1) {
      cat("Pilot setting:",
          "N_per_arm =", N_per_arm_current,
          "alphaA =", alphaA_current,
          "tau =", t_eval_fixed,
          "lambda_c =", lambda_c_fixed,
          "lambda_d =", lambda_d_fixed,
          "frailty_variance =", frailty_variance_fixed, "\n")
    }
    
    source("01_generate_data.R")
    
    pseudo_b <- MCC::GenPseudo(data = dat, tau = t_eval)
    
    AX <- unique(dat[, c("idx", "A", "X")])
    reg_dat <- merge(AX, pseudo_b[, c("idx", "pseudo")], by = "idx")
    
    # adjusted analysis
    fit_adj <- lm(pseudo ~ A + X, data = reg_dat)
    s_adj <- summary(fit_adj)$coefficients
    reject_adjusted[b] <- s_adj["A", "Pr(>|t|)"] < alpha
    
    # treatment-only analysis
    fit_trt <- lm(pseudo ~ A, data = reg_dat)
    s_trt <- summary(fit_trt)$coefficients
    reject_treatment[b] <- s_trt["A", "Pr(>|t|)"] < alpha
  }
  
  design_power$power_adjusted[j] <- mean(reject_adjusted)
  design_power$power_treatment_only[j] <- mean(reject_treatment)
  
  cat("Done:",
      "N_per_arm =", N_per_arm_current,
      "alphaA =", alphaA_current,
      "power_adjusted =", design_power$power_adjusted[j],
      "power_treatment_only =", design_power$power_treatment_only[j], "\n")
}

write.csv(design_power, "design_vignette_power_pilot.csv", row.names = FALSE)
save.image("design_vignette_power_pilot_workspace.RData")