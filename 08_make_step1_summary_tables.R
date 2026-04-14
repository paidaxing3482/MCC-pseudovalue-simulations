type1_censor <- read.csv("type1_tau_censoring_result.csv")
power_censor <- read.csv("power_tau_censoring_result.csv")
risk_censor  <- read.csv("risk_tau_censoring_summary.csv")

names(type1_censor)[names(type1_censor) == "X4t_eval"] <- "t_eval"
names(power_censor)[names(power_censor) == "X4t_eval"] <- "t_eval"
names(risk_censor)[names(risk_censor) == "X4t_eval"] <- "t_eval"


type1_censor_sub <- type1_censor[, c("t_eval", "lambda_c", "reject_rate")]
names(type1_censor_sub)[3] <- "type1"

power_censor_sub <- power_censor[, c("t_eval", "lambda_c", "power")]

risk_censor_sub <- risk_censor[, c("t_eval", "lambda_c", "risk_prop")]
names(risk_censor_sub)[3] <- "risk"


censor_long <- merge(type1_censor_sub, power_censor_sub,
                     by = c("t_eval", "lambda_c"))
censor_long <- merge(censor_long, risk_censor_sub,
                     by = c("t_eval", "lambda_c"))


lambda_vals <- sort(unique(censor_long$lambda_c))

summary_table_censoring <- data.frame(
  lambda_c = lambda_vals,
  type1_tau2 = NA_real_,
  type1_tau3 = NA_real_,
  type1_tau4 = NA_real_,
  power_tau2 = NA_real_,
  power_tau3 = NA_real_,
  power_tau4 = NA_real_,
  risk_tau2 = NA_real_,
  risk_tau3 = NA_real_,
  risk_tau4 = NA_real_
)

for (i in seq_along(lambda_vals)) {
  lam <- lambda_vals[i]
  subdat <- censor_long[censor_long$lambda_c == lam, ]
  
  summary_table_censoring$type1_tau2[i] <- subdat$type1[subdat$t_eval == 2]
  summary_table_censoring$type1_tau3[i] <- subdat$type1[subdat$t_eval == 3]
  summary_table_censoring$type1_tau4[i] <- subdat$type1[subdat$t_eval == 4]
  
  summary_table_censoring$power_tau2[i] <- subdat$power[subdat$t_eval == 2]
  summary_table_censoring$power_tau3[i] <- subdat$power[subdat$t_eval == 3]
  summary_table_censoring$power_tau4[i] <- subdat$power[subdat$t_eval == 4]
  
  summary_table_censoring$risk_tau2[i] <- subdat$risk[subdat$t_eval == 2]
  summary_table_censoring$risk_tau3[i] <- subdat$risk[subdat$t_eval == 3]
  summary_table_censoring$risk_tau4[i] <- subdat$risk[subdat$t_eval == 4]
}


type1_death <- read.csv("type1_tau_death_result.csv")
power_death <- read.csv("power_tau_death_result.csv")
risk_death  <- read.csv("risk_tau_death_summary.csv")

names(type1_death)[names(type1_death) == "X4t_eval"] <- "t_eval"
names(power_death)[names(power_death) == "X4t_eval"] <- "t_eval"
names(risk_death)[names(risk_death) == "X4t_eval"] <- "t_eval"

type1_death_sub <- type1_death[, c("t_eval", "scenario", "reject_rate")]
names(type1_death_sub)[3] <- "type1"

power_death_sub <- power_death[, c("t_eval", "scenario", "power")]

risk_death_sub <- risk_death[, c("t_eval", "scenario", "risk_prop")]
names(risk_death_sub)[3] <- "risk"

death_long <- merge(type1_death_sub, power_death_sub,
                    by = c("t_eval", "scenario"))
death_long <- merge(death_long, risk_death_sub,
                    by = c("t_eval", "scenario"))

scenario_levels <- c("baseline", "high_censor", "high_death", "high_both")

summary_table_scenarios <- data.frame(
  scenario = scenario_levels,
  type1_tau2 = NA_real_,
  type1_tau3 = NA_real_,
  type1_tau4 = NA_real_,
  power_tau2 = NA_real_,
  power_tau3 = NA_real_,
  power_tau4 = NA_real_,
  risk_tau2 = NA_real_,
  risk_tau3 = NA_real_,
  risk_tau4 = NA_real_,
  stringsAsFactors = FALSE
)

for (i in seq_along(scenario_levels)) {
  scen <- scenario_levels[i]
  subdat <- death_long[death_long$scenario == scen, ]
  
  summary_table_scenarios$type1_tau2[i] <- subdat$type1[subdat$t_eval == 2]
  summary_table_scenarios$type1_tau3[i] <- subdat$type1[subdat$t_eval == 3]
  summary_table_scenarios$type1_tau4[i] <- subdat$type1[subdat$t_eval == 4]
  
  summary_table_scenarios$power_tau2[i] <- subdat$power[subdat$t_eval == 2]
  summary_table_scenarios$power_tau3[i] <- subdat$power[subdat$t_eval == 3]
  summary_table_scenarios$power_tau4[i] <- subdat$power[subdat$t_eval == 4]
  
  summary_table_scenarios$risk_tau2[i] <- subdat$risk[subdat$t_eval == 2]
  summary_table_scenarios$risk_tau3[i] <- subdat$risk[subdat$t_eval == 3]
  summary_table_scenarios$risk_tau4[i] <- subdat$risk[subdat$t_eval == 4]
}

print(summary_table_censoring)
print(summary_table_scenarios)

write.csv(summary_table_censoring, "summary_table_censoring.csv", row.names = FALSE)
write.csv(summary_table_scenarios, "summary_table_scenarios.csv", row.names = FALSE)
