library(MCC)

set.seed(123)

B <- as.integer(Sys.getenv("B", unset = "200"))
alpha <- 0.05

tau_fixed <- 4
t_eval_fixed <- 4
lambda_c_fixed <- 0.25
lambda_d_fixed <- 0.25

alphaA_fixed <- log(1.2)
alphaX_fixed <- log(1.25)

n_per_arm_list <- unique(round(exp(seq(log(100), log(1000), length.out = 10))))
frailty_variance_list <- c(0, 1)

design_power <- expand.grid(
  N_per_arm = n_per_arm_list,
  frailty_variance = frailty_variance_list
)

design_power$power_adjusted <- NA_real_
design_power$power_treatment_only <- NA_real_
design_power$n_unique_ids <- NA_integer_
design_power$nrow_dat <- NA_integer_
design_power$nrow_pseudo <- NA_integer_
design_power$n_unique_pseudo_ids <- NA_integer_

for (j in seq_len(nrow(design_power))) {

  N_per_arm_current <- design_power$N_per_arm[j]
  frailty_variance_current <- design_power$frailty_variance[j]

  reject_adjusted <- logical(B)
  reject_treatment <- logical(B)

  for (b in seq_len(B)) {

    tau <- tau_fixed
    t_eval <- t_eval_fixed
    lambda_c <- lambda_c_fixed
    lambda_d <- lambda_d_fixed
    frailty_variance <- frailty_variance_current
    alphaA <- alphaA_fixed
    alphaX <- alphaX_fixed
    N_per_arm_external <- N_per_arm_current

    if (b == 1) {
      cat(
        "Running setting:",
        "N_per_arm =", N_per_arm_current,
        "alphaA =", alphaA_fixed,
        "alphaX =", alphaX_fixed,
        "tau =", t_eval_fixed,
        "lambda_c =", lambda_c_fixed,
        "lambda_d =", lambda_d_fixed,
        "frailty_variance =", frailty_variance_current,
        "\n"
      )
    }

    source("01_generate_data.R")

    if (b == 1) {
      design_power$n_unique_ids[j] <- length(unique(dat$idx))
      design_power$nrow_dat[j] <- nrow(dat)

      cat(
        "Size check:",
        "N_per_arm =", N_per_arm_current,
        "expected n_total =", 2 * N_per_arm_current,
        "unique ids in dat =", length(unique(dat$idx)),
        "nrow(dat) =", nrow(dat),
        "\n"
      )
    }

    pseudo_b <- MCC::GenPseudo(data = dat, tau = t_eval)

    if (b == 1) {
      design_power$nrow_pseudo[j] <- nrow(pseudo_b)
      design_power$n_unique_pseudo_ids[j] <- length(unique(pseudo_b$idx))

      cat(
        "Pseudo check:",
        "nrow(pseudo_b) =", nrow(pseudo_b),
        "unique ids in pseudo_b =", length(unique(pseudo_b$idx)),
        "\n"
      )
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

  cat(
    "Done:",
    "N_per_arm =", N_per_arm_current,
    "frailty_variance =", frailty_variance_current,
    "power_adjusted =", design_power$power_adjusted[j],
    "power_treatment_only =", design_power$power_treatment_only[j],
    "\n"
  )
}

write.csv(design_power, "design_vignette_power_final_frailty.csv", row.names = FALSE)
save.image("design_vignette_power_final_frailty_workspace.RData")

print(design_power)
