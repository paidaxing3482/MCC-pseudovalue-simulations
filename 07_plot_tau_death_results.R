type1_result <- read.csv("type1_tau_death_result.csv")
power_out <- read.csv("power_tau_death_result.csv")
risk_summary <- read.csv("risk_tau_death_summary.csv")

names(type1_result)[names(type1_result) == "X4t_eval"] <- "t_eval"
names(power_out)[names(power_out) == "X4t_eval"] <- "t_eval"
names(risk_summary)[names(risk_summary) == "X4t_eval"] <- "t_eval"

scenario_levels <- c("baseline", "high_censor", "high_death", "high_both")
scenario_labels <- c("baseline", "high cens", "high death", "both high")

type1_result$scenario <- factor(type1_result$scenario, levels = scenario_levels)
power_out$scenario <- factor(power_out$scenario, levels = scenario_levels)
risk_summary$scenario <- factor(risk_summary$scenario, levels = scenario_levels)

print(names(type1_result))
print(names(power_out))
print(names(risk_summary))

print(unique(type1_result$t_eval))
print(unique(power_out$t_eval))
print(unique(risk_summary$t_eval))

tau_vals <- sort(unique(type1_result$t_eval))
offsets <- c(-0.08, 0, 0.08)
line_types <- c(1, 2, 3)
point_types <- c(1, 2, 3)

x_base <- 1:length(scenario_levels)

plot(
  NA, NA,
  xlim = c(0.7, 4.3),
  ylim = c(0, max(type1_result$upper)),
  xlab = "Scenario",
  ylab = "Empirical Type I error",
  main = "Type I error across scenarios",
  xaxt = "n"
)

axis(1, at = x_base, labels = scenario_labels)
abline(h = 0.05, lty = 2)

for (i in seq_along(tau_vals)) {
  tau_val <- tau_vals[i]
  subdat <- type1_result[type1_result$t_eval == tau_val, ]
  subdat <- subdat[order(subdat$scenario), ]
  
  xvals <- x_base + offsets[i]
  
  lines(
    xvals, subdat$reject_rate,
    type = "b",
    lty = line_types[i],
    pch = point_types[i]
  )
  
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
  "bottomleft",
  legend = paste("tau =", tau_vals),
  lty = line_types,
  pch = point_types,
  bty = "n",
  cex = 0.9
)

tau_vals_power <- sort(unique(power_out$t_eval))

plot(
  NA, NA,
  xlim = c(0.7, 4.3),
  ylim = c(0, max(power_out$upper)),
  xlab = "Scenario",
  ylab = "Empirical power",
  main = "Power across scenarios",
  xaxt = "n"
)

axis(1, at = x_base, labels = scenario_labels)

for (i in seq_along(tau_vals_power)) {
  tau_val <- tau_vals_power[i]
  subdat <- power_out[power_out$t_eval == tau_val, ]
  subdat <- subdat[order(subdat$scenario), ]
  
  xvals <- x_base + offsets[i]
  
  lines(
    xvals, subdat$power,
    type = "b",
    lty = line_types[i],
    pch = point_types[i]
  )
  
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
  "bottomright",
  legend = paste("tau =", tau_vals_power),
  lty = line_types,
  pch = point_types,
  bty = "n",
  cex = 0.9
)


tau_vals_risk <- sort(unique(risk_summary$t_eval))

plot(
  NA, NA,
  xlim = c(0.7, 4.3),
  ylim = c(0, 1),
  xlab = "Scenario",
  ylab = "Proportion still at risk",
  main = "Risk proportion across scenarios",
  xaxt = "n"
)

axis(1, at = x_base, labels = scenario_labels)

for (i in seq_along(tau_vals_risk)) {
  tau_val <- tau_vals_risk[i]
  subdat <- risk_summary[risk_summary$t_eval == tau_val, ]
  subdat <- subdat[order(subdat$scenario), ]
  
  xvals <- x_base + offsets[i]
  
  lines(
    xvals, subdat$risk_prop,
    type = "b",
    lty = line_types[i],
    pch = point_types[i]
  )
}

legend(
  "bottomright",
  legend = paste("tau =", tau_vals_risk),
  lty = line_types,
  pch = point_types,
  bty = "n",
  cex = 0.9
)