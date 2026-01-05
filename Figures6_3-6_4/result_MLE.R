source("model_framework.R")

# This is an example for MLE fitting method with phi = 0.0083, change variables 
# and file names for phi=0.025

# Fixed parameters
gamma <- 1.4
omega <- 0.05
times <- seq(0,25,0.01)

# Change variables and file names according to the experiments
phi <- 0.0083

baseline <- read_csv("MLE/Outputs/phi_0083/MLE_baseline_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)
temp <- read_csv("MLE/Outputs/phi_0083/MLE_temp_results_Nov21.csv")|> 
  arrange(nlogll) |> 
  slice(1)
npi <- read_csv("MLE/Outputs/phi_0083/MLE_npi_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)
response <- read_csv("MLE/Outputs/phi_0083/MLE_response_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)
no_npi <- read_csv("MLE/Outputs/phi_0083/MLE_no_npi_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)
no_response <- read_csv("MLE/Outputs/phi_0083/MLE_no_response_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)
no_temp <- read_csv("MLE/Outputs/phi_0083/MLE_no_temp_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)
full <- read_csv("MLE/Outputs/phi_0083/MLE_full_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)



baseline_out <- as.data.frame(sirs_7b(beta_0= baseline$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                      k = 0, lambda_ = 0, xi = 0, 
                                      df_temp = df_temp_daily,f_npi = f_npi2, f_1 = 1,
                                      f_2 = 1, dif = baseline$dif,
                                      S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

temp_out <- as.data.frame(sirs_7b(beta_0= temp$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                  k = 0, lambda_ = 0, xi = temp$xi_fit, 
                                  df_temp = df_temp_daily,f_npi = f_npi2, f_1 =1,
                                  f_2 = 1, dif = temp$dif,
                                  S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

npi_out <- as.data.frame(sirs_7b(beta_0= npi$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                 k = 0, lambda_ =0, xi = 0, 
                                 df_temp = df_temp_daily,f_npi = f_npi2, f_1 = npi$f_1_fit,
                                 f_2 = npi$f_2_fit, dif = npi$dif,
                                 S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

response_out <- as.data.frame(sirs_7b(beta_0= response$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                      k = response$k_fit, lambda_ = response$lambda_fit, xi = 0, 
                                      df_temp = df_temp_daily,f_npi = f_npi2, f_1 = 1,
                                      f_2 = 1, dif = response$dif,
                                      S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

no_temp_out <- as.data.frame(sirs_7b(beta_0= no_temp$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                     k = no_temp$k_fit, lambda_ = no_temp$lambda_fit, xi = 0, 
                                     df_temp = df_temp_daily,f_npi = f_npi2, f_1 = no_temp$f_1_fit,
                                     f_2 = no_temp$f_2_fit, dif = no_temp$dif,
                                     S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

no_npi_out <- as.data.frame(sirs_7b(beta_0= no_npi$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                    k = no_npi$k_fit, lambda_ = no_npi$lambda_fit, xi = no_npi$xi_fit, 
                                    df_temp = df_temp_daily,f_npi = f_npi2, f_1 = 1,
                                    f_2 = 1, dif = no_npi$dif,
                                    S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

no_response_out <- as.data.frame(sirs_7b(beta_0= no_response$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                         k = 0, lambda_ = 0, xi = no_response$xi_fit, 
                                         df_temp = df_temp_daily,f_npi = f_npi2, f_1 = no_response$f_1_fit,
                                         f_2 = no_response$f_2_fit, dif = no_response$dif,
                                         S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

full_out <- as.data.frame(sirs_7b(beta_0= full$beta_0_fit, gamma = gamma, omega = omega, phi = phi,
                                  k = full$k_fit, lambda_ = full$lambda_fit, xi = full$xi_fit, 
                                  df_temp = df_temp_daily,f_npi = f_npi2, f_1 = full$f_1_fit,
                                  f_2 = full$f_2_fit, dif = full$dif,
                                  S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

library(cowplot)

# Create a combined data frame
plot_data <- tibble(
  time = times,
  baseline = baseline_out,
  temperature = temp_out,
  npi = npi_out,
  response = response_out,
  no_temperature = no_temp_out,
  no_npi = no_npi_out,
  no_response = no_response_out,
  full = full_out
) %>%
  pivot_longer(
    cols = -time,
    names_to = "scenario", 
    values_to = "deaths"
  )

# Create individual plots without legends
create_plot <- function(scenario_name, title) {
  ggplot() +
    geom_line(data = plot_data %>% filter(scenario == scenario_name),
              aes(x = time, y = deaths), color = "black", linewidth = 1) +
    geom_point(data = df, aes(x = time, y = d_inc), 
               color = "red", alpha = 0.6) +
    labs(title = title, x = "Time", y = "Deaths") +
    theme_minimal_hgrid() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 25),  # Title size (already set)
      axis.title = element_text(size = 22),             # Axis title size
      axis.text = element_text(size = 22),              # Axis numbers size
      axis.title.x = element_text(margin = margin(t = 10)),  # Optional: space below x-axis
      axis.title.y = element_text(margin = margin(r = 10))   # Optional: space right of y-axis
    )
}

# Create all plots
p1 <- create_plot("baseline", "Baseline")
p2 <- create_plot("temperature", "Temperature") 
p3 <- create_plot("npi", "NPI")
p4 <- create_plot("response", "Response")
p5 <- create_plot("no_temperature", "No Temperature")
p6 <- create_plot("no_npi", "No NPI")
p7 <- create_plot("no_response", "No Response")
p8 <- create_plot("full", "Full")

# Create a separate legend with larger font
legend_plot <- ggplot() +
  geom_line(aes(x = 1, y = 1, color = "Fitted")) +
  geom_point(aes(x = 1, y = 1, color = "Observed")) +
  scale_color_manual(values = c("Fitted" = "black", "Observed" = "red"),
                     name = "") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.key = element_rect(fill = "white"),
    legend.text = element_text(size = 22),      # Legend text size
    legend.title = element_text(size = 22)      # Legend title size
  )

# Extract the legend
legend <- get_legend(legend_plot)

# Combine all plots and add single legend at the bottom
combined_plot <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, 
                           ncol = 4, 
                           nrow = 2,
                           labels = "AUTO",
                           label_size = 22)

# Add the single legend at the bottom
final_plot <- plot_grid(combined_plot, legend, 
                        ncol = 1, 
                        rel_heights = c(1, 0.08))  # Increased from 0.05 for larger legend

# Display the plot
print(final_plot)

# ggsave("MLE_results_phi_0083.png", final_plot,
#        width = 16, height = 10, dpi = 600)
