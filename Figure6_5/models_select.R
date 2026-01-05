source("model_framework.R")

gamma <- 1.4
omega <- 0.05
times <- seq(0,25,0.01)

# MLE
response_025 <- read_csv("MLE/Outputs/phi_025/MLE_response_results_Nov23.csv") |> 
  arrange(nlogll) |> 
  slice(1)

full_025 <- read_csv("MLE/Outputs/phi_025/MLE_full_results_Nov23.csv") |> 
  arrange(nlogll) |> 
  slice(1)


temp_0083 <- read_csv("MLE/Outputs/phi_0083/MLE_temp_results_Nov21.csv")|> 
  arrange(nlogll) |> 
  slice(1)

full_0083 <- read_csv("MLE/Outputs/phi_0083/MLE_full_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)

# Run models
response_out_025 <- as.data.frame(sirs_7b(beta_0= response_025$beta_0_fit, gamma = gamma, omega = omega, phi = 0.025,
                                      k = response_025$k_fit, lambda_ = response_025$lambda_fit, xi = 0, 
                                      df_temp = df_temp_daily,f_npi = f_npi2, f_1 = 1,
                                      f_2 = 1, dif = response_025$dif,
                                      S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

full_out_025 <- as.data.frame(sirs_7b(beta_0= full_025$beta_0_fit, gamma = gamma, omega = omega, phi = 0.025,
                                  k = full_025$k_fit, lambda_ = full_025$lambda_fit, xi = full_025$xi_fit, 
                                  df_temp = df_temp_daily,f_npi = f_npi2, f_1 = full_025$f_1_fit,
                                  f_2 = full_025$f_2_fit, dif = full_025$dif,
                                  S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc


temp_out_0083 <- as.data.frame(sirs_7b(beta_0= temp_0083$beta_0_fit, gamma = gamma, omega = omega, phi = 0.0083,
                                  k = 0, lambda_ = 0, xi = temp_0083$xi_fit, 
                                  df_temp = df_temp_daily,f_npi = f_npi2, f_1 =1,
                                  f_2 = 1, dif = temp$dif,
                                  S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

full_out_0083 <- as.data.frame(sirs_7b(beta_0= full_0083$beta_0_fit, gamma = gamma, omega = omega, phi = 0.0083,
                                  k = full_0083$k_fit, lambda_ = full_0083$lambda_fit, xi = full_0083$xi_fit, 
                                  df_temp = df_temp_daily,f_npi = f_npi2, f_1 = full_0083$f_1_fit,
                                  f_2 = full_0083$f_2_fit, dif = full_0083$dif,
                                  S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))$d_inc

library(cowplot)

# Create a combined data frame
plot_data <- tibble(
  time = times,
  temperature_0083 = temp_out_0083,
  response_025 = response_out_025,
  full_025 = full_out_025,
  full_0083 = full_out_0083
) %>%
  pivot_longer(
    cols = -time,
    names_to = "scenario",
    values_to = "deaths"
  ) %>%
  mutate(
    phi = case_when(
      str_detect(scenario, "0083$") ~ 0.0083,
      str_detect(scenario, "025$")  ~ 0.025,
      TRUE ~ NA_real_
    )
  )

create_plot <- function(scenario_name, title_text, phi_value) {
  ggplot() +
    geom_line(data = plot_data %>% filter(scenario == scenario_name),
              aes(x = time, y = deaths), color = "black", linewidth = 1.5) +
    geom_point(data = df, aes(x = time, y = d_inc), 
               color = "red", alpha = 0.6, size = 2) +
    labs(title = parse(text = paste0('bold("', title_text, '")', ' ~ ϕ == ', phi_value)), 
         x = "Time", y = "Deaths") +
    theme_minimal_hgrid() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 25),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 22)
    )
}

# Create the four individual plots
p1 <- create_plot("response_025", "Response", "0.025")
p2 <- create_plot("full_025", "Full", "0.025")
p3 <- create_plot("temperature_0083", "Temperature", "0.0083")
p4 <- create_plot("full_0083", "Full", "0.0083")

# Create ONE BIG 2x2 grid with all plots
combined_plot <- plot_grid(p1, p2, p3, p4,
                           ncol = 2, 
                           nrow = 2,
                           labels = c("A", "B", "C", "D"),
                           label_size = 24)

# Add legend
legend_plot <- ggplot() +
  geom_line(aes(x = 1, y = 1, color = "Fitted")) +
  geom_point(aes(x = 1, y = 1, color = "Observed")) +
  scale_color_manual(values = c("Fitted" = "black", "Observed" = "red"),
                     name = "") +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 22)
  )

legend <- get_legend(legend_plot)

# Add the single legend at the bottom
final_plot <- plot_grid(combined_plot, legend,
                        ncol = 1,
                        rel_heights = c(1, 0.08))

# Display the plot
final_plot


# Save the plot
# ggsave("comparison_plots.png", final_plot,
#        width = 16, height = 10, dpi = 600)  
