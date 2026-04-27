library(MCC)

set.seed(123)

B <- 500
alpha <- 0.05

tau_fixed <- 10
t_eval_fixed <- 4

lambda_c_fixed <- 0.25
lambda_d_fixed <- 0.25
frailty_variance_fixed <- 0

alphaX_fixed <- log(1.25)
alphaA_fixed <- log(1.2)

n_per_arm_list <- c(900, 950, 1000)

design_power <- data.frame(
  N_per_arm = n_per_arm_list,
  power_adjusted = NA_real_,
  power_treatment_only = NA_real_,
  n_unique_ids = NA_real_,
  nrow_dat = NA_real_,
  nrow_pseudo = NA_real_,
  n_unique_pseudo_ids = NA_real_
)

for (j in seq_len(nrow(design_power))) {
  
  N_per_arm_current <- design_power$N_per_arm[j]
  
  reject_adjusted <- logical(B)
  reject_treatment <- logical(B)
  
  for (b in 1:B) {
    
    rm(
      list = c(
        "tau", "lambda_c", "lambda_d", "frailty_variance",
        "alphaA", "alphaX", "N_per_arm", "N_per_arm_external", "n_total",
        "m", "base_event_rate", "A", "X", "covariates",
        "beta_event", "beta_death", "dat",
        "events_per_id", "death_per_id"
      ),
      envir = .GlobalEnv
    )
    
    tau <- tau_fixed
    N_per_arm_external <- N_per_arm_current
    alphaA <- alphaA_fixed
    alphaX <- alphaX_fixed
    lambda_c <- lambda_c_fixed
    lambda_d <- lambda_d_fixed
    frailty_variance <- frailty_variance_fixed
    t_eval <- t_eval_fixed
    
    if (b == 1) {
      cat("Refined setting:",
          "N_per_arm =", N_per_arm_current,
          "alphaA =", alphaA_fixed,
          "tau =", t_eval_fixed,
          "lambda_c =", lambda_c_fixed,
          "lambda_d =", lambda_d_fixed,
          "frailty_variance =", frailty_variance_fixed, "\n")
    }
    
    source("01_generate_data.R")
    
    if (b == 1) {
      design_power$n_unique_ids[j] <- length(unique(dat$idx))
      design_power$nrow_dat[j] <- nrow(dat)
      
      cat("Size check:",
          "N_per_arm =", N_per_arm_current,
          "expected n_total =", 2 * N_per_arm_current,
          "unique ids in dat =", length(unique(dat$idx)),
          "nrow(dat) =", nrow(dat), "\n")
    }
    
    pseudo_b <- MCC::GenPseudo(data = dat, tau = t_eval)
    
    if (b == 1) {
      design_power$nrow_pseudo[j] <- nrow(pseudo_b)
      design_power$n_unique_pseudo_ids[j] <- length(unique(pseudo_b$idx))
      
      cat("Pseudo check:",
          "nrow(pseudo_b) =", nrow(pseudo_b),
          "unique ids in pseudo_b =", length(unique(pseudo_b$idx)), "\n")
    }
    
    AX <- unique(dat[, c("idx", "A", "X")])
    reg_dat <- merge(AX, pseudo_b[, c("idx", "pseudo")], by = "idx")
    
    fit_adj <- lm(pseudo ~ A + X, data = reg_dat)
    s_adj <- summary(fit_adj)$coefficients
    reject_adjusted[b] <- s_adj["A", "Pr(>|t|)"] < alpha
    
    fit_trt <- lm(pseudo ~ A, data = reg_dat)
    s_trt <- summary(fit_trt)$coefficients
    reject_treatment[b] <- s_trt["A", "Pr(>|t|)"] < alpha
  }
  
  design_power$power_adjusted[j] <- mean(reject_adjusted)
  design_power$power_treatment_only[j] <- mean(reject_treatment)
  
  cat("Done:",
      "N_per_arm =", N_per_arm_current,
      "power_adjusted =", design_power$power_adjusted[j],
      "power_treatment_only =", design_power$power_treatment_only[j], "\n")
}

write.csv(design_power, "design_vignette_power_confirm.csv", row.names = FALSE)
save.image("design_vignette_power_confirm_workspace.RData")
