library(ggplot2)

res <- read.csv("design_vignette_power_final_frailty.csv")

power_long <- rbind(
  data.frame(
    N_per_arm = res$N_per_arm,
    frailty_variance = res$frailty_variance,
    power = res$power_adjusted,
    analysis = "Covariate-adjusted"
  ),
  data.frame(
    N_per_arm = res$N_per_arm,
    frailty_variance = res$frailty_variance,
    power = res$power_treatment_only,
    analysis = "Treatment-only"
  )
)

power_long$frailty_label <- ifelse(
  power_long$frailty_variance == 0,
  "No frailty",
  "Shared frailty variance = 1"
)

dir.create("figures", showWarnings = FALSE)

p <- ggplot(
  power_long,
  aes(
    x = N_per_arm,
    y = power,
    group = analysis,
    linetype = analysis,
    shape = analysis
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.80, linetype = "dashed") +
  scale_x_continuous(
    limits = c(100, 1000),
    breaks = seq(100, 1000, by = 100)
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(~ frailty_label) +
  labs(
    title = "Design vignette power at tau = 4",
    x = "Sample size per arm",
    y = "Empirical power",
    linetype = "Analysis",
    shape = "Analysis"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

ggsave("figures/design_vignette_power_final_frailty.png", p, width = 8, height = 4.5, dpi = 300)
ggsave("figures/design_vignette_power_final_frailty.pdf", p, width = 8, height = 4.5)

cat("Saved final plots with linear x-axis:\n")
cat("figures/design_vignette_power_final_frailty.png\n")
cat("figures/design_vignette_power_final_frailty.pdf\n")
