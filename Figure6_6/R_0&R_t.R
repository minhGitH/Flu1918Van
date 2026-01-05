source("model_framework.R")
library(cowplot)

gamma <- 1.4
omega <- 0.05
times <- seq(0,25,0.01)
temp_fit <- loess(temp ~ time, data = df_temp_daily)
temp_val <- predict(temp_fit, newdata = data.frame(time = times))


# R_0 calculation ---------------------------------------------------------------------

R_0 <- function(theta) {
  return(mean(theta$beta_0_fit*exp(-theta$xi_fit*temp_val))/gamma)
}

# LSE
full_LSE_025 <- read_csv("LSE/Outputs/phi_025/LSE_full_results_Nov4.csv") |> 
  arrange(rmse) |> 
  slice(1)

full_LSE_0083 <- read_csv("LSE/Outputs/phi_0083/LSE_full_results_Nov9.csv") |> 
  arrange(rmse) |> 
  slice(1)



# Run models
# 
full_LSE_out_025 <- as.data.frame(sirs_7b(beta_0= full_LSE_025$beta_0_fit, gamma = gamma, omega = omega, phi = 0.025,
                                          k = full_LSE_025$k_fit, lambda_ = full_LSE_025$lambda_fit, xi = full_LSE_025$xi_fit,
                                          df_temp = df_temp_daily,f_npi = f_npi2, f_1 = full_LSE_025$f_1_fit,
                                          f_2 = full_LSE_025$f_2_fit, dif = full_LSE_025$dif,
                                          S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))


# 
full_LSE_out_0083 <- as.data.frame(sirs_7b(beta_0= full_LSE_0083$beta_0_fit, gamma = gamma, omega = omega, phi = 0.0083,
                                           k = full_LSE_0083$k_fit, lambda_ = full_LSE_0083$lambda_fit, xi = full_LSE_0083$xi_fit,
                                           df_temp = df_temp_daily,f_npi = f_npi2, f_1 = full_LSE_0083$f_1_fit,
                                           f_2 = full_LSE_0083$f_2_fit, dif = full_LSE_0083$dif,
                                           S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))

R_0_full_LSE_025 <- R_0(full_LSE_025)
# 6.333872

R_0_full_LSE_0083 <- R_0(full_LSE_0083)
# 4.493294

# MLE

full_MLE_025 <- read_csv("MLE/Outputs/phi_025/MLE_full_results_Nov23.csv") |> 
  arrange(nlogll) |> 
  slice(1)

full_MLE_0083 <- read_csv("MLE/Outputs/phi_0083/MLE_full_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)

# # Run models
# 
full_MLE_out_025 <- as.data.frame(sirs_7b(beta_0= full_MLE_025$beta_0_fit, gamma = gamma, omega = omega, phi = 0.025,
                                          k = full_MLE_025$k_fit, lambda_ = full_MLE_025$lambda_fit, xi = full_MLE_025$xi_fit,
                                          df_temp = df_temp_daily,f_npi = f_npi2, f_1 = full_MLE_025$f_1_fit,
                                          f_2 = full_MLE_025$f_2_fit, dif = full_MLE_025$dif,
                                          S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))


full_MLE_out_0083 <- as.data.frame(sirs_7b(beta_0= full_MLE_0083$beta_0_fit, gamma = gamma, omega = omega, phi = 0.0083,
                                           k = full_MLE_0083$k_fit, lambda_ = full_MLE_0083$lambda_fit, xi = full_MLE_0083$xi_fit,
                                           df_temp = df_temp_daily,f_npi = f_npi2, f_1 = full_MLE_0083$f_1_fit,
                                           f_2 = full_MLE_0083$f_2_fit, dif = full_MLE_0083$dif,
                                           S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))

R_0_full_MLE_025 <- R_0(full_MLE_025)
# 6.425395

R_0_full_MLE_0083 <- R_0(full_MLE_0083)
# 3.738466


# R_t plots ---------------------------------------------------------------

# These are actually R_t values (beta/gamma)
Rt_full_LSE_025 <- full_LSE_out_025$beta.1/gamma  # Should be renamed to R_t
Rt_full_LSE_0083 <- full_LSE_out_0083$beta.1/gamma
Rt_full_MLE_025 <- full_MLE_out_025$beta.1/gamma
Rt_full_MLE_0083 <- full_MLE_out_0083$beta.1/gamma

# Create a combined data frame with proper categorization
plot_data <- tibble(
  time = times,  # assuming all vectors have same length
  Rt_LSE_025 = Rt_full_LSE_025,  # Fixed naming
  Rt_LSE_0083 = Rt_full_LSE_0083,
  Rt_MLE_025 = Rt_full_MLE_025,
  Rt_MLE_0083 = Rt_full_MLE_0083
) %>%
  pivot_longer(
    cols = -time,
    names_to = "scenario",
    values_to = "R_t"  # Consistent naming
  ) %>%
  mutate(
    # Extract method using str_detect
    method = case_when(
      str_detect(scenario, "LSE") ~ "LSE",
      str_detect(scenario, "MLE") ~ "MLE",
      TRUE ~ "Unknown"
    ),
    # Extract parameter value using str_detect
    phi = case_when(
      str_detect(scenario, "_025$") ~ 0.025,
      str_detect(scenario, "_0083$") ~ 0.0083,
      TRUE ~ NA_real_
    ),
    # Create scenario_name for filtering
    scenario_name = paste(method, phi)
  )

# Define a function to create plots
create_Rt_plot <- function(method_name, phi) {
  
  # Filter data for this specific scenario
  plot_df <- plot_data %>% 
    filter(method == method_name, phi == !!phi)
  
  # Create color based on method
  line_color <- case_when(
    method_name == "LSE" ~ "blue",
    method_name == "MLE" ~ "red",
    TRUE ~ "black"
  )
  
  ggplot(plot_df, aes(x = time, y = R_t)) +  # Using R_t
    geom_line(color = line_color, linewidth = 1.5) +
    labs(
      title = parse(text = paste0('bold("', method_name, '")', ' ~ ϕ == ', phi)),
      x = "Time",
      y = expression(R[t])  # Correct mathematical notation
    ) +
    theme_minimal_hgrid() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 25),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 22)
    )
}

# Create the four individual plots
p1 <- create_Rt_plot("LSE", 0.025)
p2 <- create_Rt_plot("LSE", 0.0083)
p3 <- create_Rt_plot("MLE", 0.025)
p4 <- create_Rt_plot("MLE", 0.0083)

# Create the 2x2 grid with all plots
combined_plot <- plot_grid(p1, p2, p3, p4,
                           ncol = 2,
                           nrow = 2,
                           labels = c("A", "B", "C", "D"),
                           label_size = 24)

# Add legend (color coding by method)
legend_plot <- ggplot() +
  geom_line(aes(x = 1, y = 1, color = "LSE")) +
  geom_line(aes(x = 1, y = 1.5, color = "MLE")) +
  scale_color_manual(values = c("LSE" = "blue", "MLE" = "red"),
                     name = "Method") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22)
  )

legend <- get_legend(legend_plot)

# Add the legend at the bottom
final_plot <- plot_grid(combined_plot, legend,
                        ncol = 1,
                        rel_heights = c(1, 0.08))

# Display the plot
final_plot

# Save the plot with correct naming
# ggsave("Rt_comparison_plot.png", final_plot,
#        width = 16, height = 10, dpi = 600)