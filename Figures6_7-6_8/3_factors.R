source("model_framework.R")
library(cowplot)

gamma <- 1.4
omega <- 0.05
times <- seq(0,25,0.01)
temp_fit <- loess(temp ~ time, data = df_temp_daily)
temp_val <- predict(temp_fit, newdata = data.frame(time = times))

# 3 factor roles in beta --------------------------------------------------

# k*1000 because it was standardized
# 100010 is the total population N

# LSE

beta_LSE_0083 <- full_LSE_out_0083$beta.1
temp_LSE_0083 <- exp(-full_LSE_0083$xi_fit * temp_val)
npi_LSE_0083 <- f_npi2(time = times, 
                       f_1 = full_LSE_0083$f_1_fit,
                       f_2 = full_LSE_0083$f_2_fit, 
                       dif = full_LSE_0083$dif)
response_LSE_0083 <- (1 - full_LSE_out_0083[, 6] / 100010)^(full_LSE_0083$k_fit*1000)

# Create data frame for plotting
plot_data <- tibble(
  time = times,
  beta = beta_LSE_0083,
  temperature = temp_LSE_0083,
  npi = npi_LSE_0083,
  response = response_LSE_0083
)

# Create plot A: β(t) - top row, full width
p_beta <- ggplot(plot_data, aes(x = time, y = beta)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = expression("Transmission rate" ~ beta(t)),
       x = "Time",  # Added Time
       y = expression(beta(t))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    panel.grid.minor = element_blank()
  )

# Create plot B: Temperature
p_temp <- ggplot(plot_data, aes(x = time, y = temperature)) +
  geom_line(color = "darkgreen", linewidth = 1.5) +
  labs(title = "Temperature",
       x = "Time",  # Added Time
       y = expression(e^{-xi %.% T(t)})) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
  )

# Create plot C: NPI
p_npi <- ggplot(plot_data, aes(x = time, y = npi)) +
  geom_line(color = "darkred", linewidth = 1.5) +
  labs(title = "NPI",
       x = "Time",  # Added Time
       y = expression(H(t))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
  )

# Create plot D: Human Response
p_response <- ggplot(plot_data, aes(x = time, y = response)) +
  geom_line(color = "purple", linewidth = 1.5) +
  labs(title = "Human Response",
       x = "Time",  # Already had Time
       y = expression((1 - P(t)/N)^{k})) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
  )

# Create the bottom row with 3 subplots
bottom_row <- plot_grid(p_temp, p_npi, p_response,
                        ncol = 3,
                        align = 'h',
                        labels = c("B", "C", "D"),
                        label_size = 24)

# Combine with beta plot on top
final_plot <- plot_grid(p_beta, bottom_row,
                        ncol = 1,
                        rel_heights = c(1, 0.8),
                        labels = c("A", ""),
                        label_size = 24)

# Display the plot
print(final_plot)

# # Save with larger dimensions
# ggsave("LSE_model_factors.png", final_plot,
#        width = 16, height = 10, dpi = 600)


# MLE

beta_MLE_0083 <- full_MLE_out_0083$beta.1
temp_MLE_0083 <- exp(-full_MLE_0083$xi_fit * temp_val)
npi_MLE_0083 <- f_npi2(time = times, 
                       f_1 = full_MLE_0083$f_1_fit,
                       f_2 = full_MLE_0083$f_2_fit, 
                       dif = full_MLE_0083$dif)
response_MLE_0083 <- (1 - full_MLE_out_0083[, 6] / 100010)^(full_MLE_0083$k_fit*1000)

# Create data frame for plotting
plot_data <- tibble(
  time = times,
  beta = beta_MLE_0083,
  temperature = temp_MLE_0083,
  npi = npi_MLE_0083,
  response = response_MLE_0083
)

# Create plot A: β(t) - top row, full width
p_beta <- ggplot(plot_data, aes(x = time, y = beta)) +
  geom_line(color = "darkblue", linewidth = 1.5) +
  labs(title = expression("Transmission rate" ~ beta(t)),
       x = "Time",  # Added Time
       y = expression(beta(t))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20),
    panel.grid.minor = element_blank()
  )

# Create plot B: Temperature
p_temp <- ggplot(plot_data, aes(x = time, y = temperature)) +
  geom_line(color = "darkgreen", linewidth = 1.5) +
  labs(title = "Temperature",
       x = "Time",  # Added Time
       y = expression(e^{-xi %.% T(t)})) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
  )

# Create plot C: NPI
p_npi <- ggplot(plot_data, aes(x = time, y = npi)) +
  geom_line(color = "darkred", linewidth = 1.5) +
  labs(title = "NPI",
       x = "Time",  # Added Time
       y = expression(H(t))) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
  )

# Create plot D: Human Response
p_response <- ggplot(plot_data, aes(x = time, y = response)) +
  geom_line(color = "purple", linewidth = 1.5) +
  labs(title = "Human Response",
       x = "Time",  # Already had Time
       y = expression((1 - P(t)/N)^{k})) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
    axis.title = element_text(size = 22),
    axis.text = element_text(size = 20)
  )

# Create the bottom row with 3 subplots
bottom_row <- plot_grid(p_temp, p_npi, p_response,
                        ncol = 3,
                        align = 'h',
                        labels = c("B", "C", "D"),
                        label_size = 24)

# Combine with beta plot on top
final_plot <- plot_grid(p_beta, bottom_row,
                        ncol = 1,
                        rel_heights = c(1, 0.8),
                        labels = c("A", ""),
                        label_size = 24)

# Display the plot
print(final_plot)

# # Save with larger dimensions
# ggsave("MLE_model_factors.png", final_plot,
#        width = 16, height = 10, dpi = 600)