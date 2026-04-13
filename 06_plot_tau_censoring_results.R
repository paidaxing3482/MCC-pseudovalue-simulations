# 06_plot_tau_censoring_results.R



type1_result <- read.csv("type1_tau_censoring_result.csv")
power_out <- read.csv("power_tau_censoring_result.csv")
risk_summary <- read.csv("risk_tau_censoring_summary.csv")


names(type1_result)[names(type1_result) == "X4t_eval"] <- "t_eval"
names(power_out)[names(power_out) == "X4t_eval"] <- "t_eval"
names(risk_summary)[names(risk_summary) == "X4t_eval"] <- "t_eval"


print(names(type1_result))
print(names(power_out))
print(names(risk_summary))


print(unique(type1_result$t_eval))
print(unique(power_out$t_eval))
print(unique(risk_summary$t_eval))

tau_vals <- sort(unique(type1_result$t_eval))
offsets <- c(-0.005, 0, 0.005)

# Figure 1: Type I error vs censoring

plot(
  NA, NA,
  xlim = c(min(type1_result$lambda_c) - 0.01,
           max(type1_result$lambda_c) + 0.01),
  ylim = c(0, max(type1_result$upper)),
  xlab = "Censoring rate parameter (lambda_c)",
  ylab = "Empirical Type I error",
  main = "Type I error vs censoring"
)

abline(h = 0.05, lty = 2)

for (i in seq_along(tau_vals)) {
  tau_val <- tau_vals[i]
  
  subdat <- type1_result[type1_result$t_eval == tau_val, ]
  subdat <- subdat[order(subdat$lambda_c), ]
  
  xvals <- subdat$lambda_c + offsets[i]
  
  lines(xvals, subdat$reject_rate, type = "b")
  
  arrows(
    x0 = xvals,
    y0 = subdat$lower,
    x1 = xvals,
    y1 = subdat$upper,
    angle = 90,
    code = 3,
    length = 0.05
  )
}

legend(
  "topleft",
  legend = paste("tau =", tau_vals),
  lty = 1,
  pch = 1,
  bty = "n"
)




# Figure 2: Power vs censoring



tau_vals_power <- sort(unique(power_out$t_eval))

plot(
  NA, NA,
  xlim = c(min(power_out$lambda_c) - 0.01,
           max(power_out$lambda_c) + 0.01),
  ylim = c(0, max(power_out$upper)),
  xlab = "Censoring rate parameter (lambda_c)",
  ylab = "Empirical power",
  main = "Power vs censoring"
)

for (i in seq_along(tau_vals_power)) {
  tau_val <- tau_vals_power[i]
  
  subdat <- power_out[power_out$t_eval == tau_val, ]
  subdat <- subdat[order(subdat$lambda_c), ]
  
  xvals <- subdat$lambda_c + offsets[i]
  
  lines(xvals, subdat$power, type = "b")
  
  arrows(
    x0 = xvals,
    y0 = subdat$lower,
    x1 = xvals,
    y1 = subdat$upper,
    angle = 90,
    code = 3,
    length = 0.05
  )
}

legend(
  "topleft",
  legend = paste("tau =", tau_vals_power),
  lty = 1,
  pch = 1,
  bty = "n"
)


# Figure 3: Risk proportion vs censoring

tau_vals_risk <- sort(unique(risk_summary$t_eval))

plot(
  NA, NA,
  xlim = c(min(risk_summary$lambda_c) - 0.01,
           max(risk_summary$lambda_c) + 0.01),
  ylim = c(0, 1),
  xlab = "Censoring rate parameter (lambda_c)",
  ylab = "Proportion still at risk",
  main = "Risk proportion vs censoring"
)

for (i in seq_along(tau_vals_risk)) {
  tau_val <- tau_vals_risk[i]
  
  subdat <- risk_summary[risk_summary$t_eval == tau_val, ]
  subdat <- subdat[order(subdat$lambda_c), ]
  
  xvals <- subdat$lambda_c + offsets[i]
  
  lines(xvals, subdat$risk_prop, type = "b")
}

legend(
  "bottomleft",
  legend = paste("tau =", tau_vals_risk),
  lty = 1,
  pch = 1,
  bty = "n"
)

