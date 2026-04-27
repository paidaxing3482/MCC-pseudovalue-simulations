library(ggplot2)

# Read confirmatory design-vignette result
power_confirm <- read.csv("design_vignette_power_confirm.csv")

# Convert from wide to long format without extra packages
power_long <- data.frame(
  N_per_arm = rep(power_confirm$N_per_arm, times = 2),
  power = c(power_confirm$power_adjusted,
            power_confirm$power_treatment_only),
  analysis = rep(c("Covariate-adjusted", "Treatment-only"),
                 each = nrow(power_confirm))
)

# Print to check
print(power_confirm)
print(power_long)

# Plot
p <- ggplot(power_long,
            aes(x = N_per_arm,
                y = power,
                group = analysis,
                linetype = analysis,
                shape = analysis)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  geom_hline(yintercept = 0.80, linetype = "dashed") +
  scale_y_continuous(limits = c(0.7, 0.9)) +
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

print(p)

# Save figure
ggsave("design_vignette_power_confirm.pdf", p, width = 6, height = 4)
ggsave("design_vignette_power_confirm.png", p, width = 6, height = 4, dpi = 300)
