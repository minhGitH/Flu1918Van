source("model_framework.R")

# Temp fig
library(splines)

p <- df_temp_daily |>
  ggplot(aes(x = date, y = temp)) +
  geom_line(aes(color = "Observed")) +
  geom_point(aes(color = "Observed")) +
  geom_smooth(aes(color = "LOESS fit"), method = "loess", se = FALSE, linewidth = 1.5) +
  geom_smooth(aes(color = "B-spline fit"), method = "lm", formula = y ~ bs(x, df = 6), se = FALSE, linewidth = 1.5) +
  scale_color_manual(
    name = "Trend type",
    values = c("Observed" = "black", "LOESS fit" = "green", "B-spline fit" = "blue")
  ) +
  labs(x = "Date", y = "Temperature (°C)") +
  theme_bw(base_size = 28) +
  theme(
    axis.text = element_text(size = 28),
    axis.title = element_text(size = 32, face = "bold"),
    legend.title = element_text(size = 30, face = "bold"),
    legend.text = element_text(size = 26),
    legend.position = c(0.9, 0.9)
  )

p

# ggsave("temp_plot.png", p, width = 16, height = 9, dpi = 600)