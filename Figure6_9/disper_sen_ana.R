source("model_framework.R")
library(lme4)
library(snow)
library(pbapply)

# Your dispersion sensitivity function
nll_dispersion = function(dispersion, params) {
  # Unpack parameters
  beta_0 <- params$beta_0
  xi <- params$xi
  k <- params$k
  lambda_ <- params$lambda_
  f_1 <- params$f_1
  f_2 <- params$f_2
  dif <- params$dif
  
  # Run model
  out  <- as.data.frame(sirs_7b(
    beta_0 = beta_0, gamma = params$gamma, omega = params$omega, 
    phi = params$phi, k = k, lambda_ = lambda_, xi = xi, 
    df_temp = params$df_temp_daily, f_npi = params$f_npi2, 
    f_1 = f_1, f_2 = f_2, dif = dif,
    S0 = 100000, I0 = 10, R0 = 0, D0 = 0, P0 = 0, 
    times = params$times
  ))
  
  # Calculate negative log-likelihood
  return(-sum(dnbinom(params$df$d_inc, size = 1/dispersion, mu = out$d_inc, log = TRUE)))
}

# --- Parameter setup (from your MLE results) ---
gamma <- 1.4
omega <- 0.05
phi <- 0.0083
times <- seq(0, 25, 1)

# Load your fitted parameters
full_MLE_0083 <- read_csv("MLE/Outputs/phi_0083/MLE_full_results_Nov21.csv") |> 
  arrange(nlogll) |> 
  slice(1)

# Bundle all parameters into a list (like in your example)
params_list <- list(
  beta_0 = full_MLE_0083$beta_0_fit,
  xi = full_MLE_0083$xi_fit,
  k = full_MLE_0083$k_fit,
  lambda_ = full_MLE_0083$lambda_fit,
  f_1 = full_MLE_0083$f_1_fit,
  f_2 = full_MLE_0083$f_2_fit,
  dif = full_MLE_0083$dif,  # Assuming dif = 1 for your case, adjust if needed
  gamma = gamma,
  omega = omega,
  phi = phi,
  df_temp_daily = df_temp_daily,
  f_npi2 = f_npi2,
  times = times,
  df = df
)

# --- Dispersion values to test ---
dis_list <- seq(0.1, 10, 0.1)  # 100 values from 0.1 to 10

# --- Create cluster ---
num_cores <- 11  # Specify your number of cores
cl <- makeCluster(num_cores, type = "SOCK")

# --- Export needed variables to workers ---

clusterExport(
  cl,
  c("nll_dispersion", "sirs_7b", "df_temp_daily", "f_npi2",
    "gamma", "omega", "phi", "params_list",
    "df", "times","dis_list")
)


# Optionally load packages on workers (if your model needs them)
clusterEvalQ(cl, {
  library(tidyverse)
  library(deSolve)
  # library(lme4)
})

# --- Function to evaluate one dispersion value ---
run_dispersion_eval <- function(i) {
  dispersion <- dis_list[i]
  
  tryCatch({
    nll_value <- nll_dispersion(dispersion, params_list)
    
    tibble(
      dispersion = dispersion,
      nll = nll_value,
      success = TRUE
    )
  }, error = function(e) {
    message("Error at dispersion = ", dispersion, ": ", e$message)
    
    tibble(
      dispersion = dispersion,
      nll = NA_real_,
      success = FALSE,
      error_msg = as.character(e$message)
    )
  })
}

# --- Run in parallel with progress bar ---
cat("Running dispersion sensitivity analysis with", num_cores, "cores...\n")
run_time <- system.time({
  results_list <- pblapply(
    seq_along(dis_list), 
    run_dispersion_eval, 
    cl = cl
  )
})

# --- Stop cluster ---
stopCluster(cl)

cat("Parallel computation time:", round(run_time[3], 2), "seconds\n")

# --- Combine results ---
results_df <- bind_rows(results_list)

# Filter out failed runs
valid_results <- results_df |> 
  filter(success == TRUE) |> 
  select(dispersion, nll)

cat("Successful evaluations:", nrow(valid_results), "/", length(dis_list), "\n")

# --- Find optimal dispersion ---
if (nrow(valid_results) > 0) {
  optimal_idx <- which.min(valid_results$nll)
  optimal_disp <- valid_results$dispersion[optimal_idx]
  min_nll <- valid_results$nll[optimal_idx]
  
  cat("Optimal dispersion:", round(optimal_disp, 4), "\n")
  cat("Minimum NLL:", round(min_nll, 4), "\n")
} else {
  cat("No successful evaluations!\n")
  # Debug: show first few errors
  print(head(results_df |> filter(success == FALSE)))
}

# --- Create plot ---
if (nrow(valid_results) > 0) {
  p <- ggplot(valid_results, aes(x = dispersion, y = nll)) +
    geom_line(color = "blue", linewidth = 1.2) +
    geom_point(color = "red", size = 2, alpha = 0.6) +
    labs(
      title = bquote("Full Model with" ~ varphi == 0.0083),
      x = "Dispersion Parameter",
      y = "Negative Log Likelihood"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 25, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 22),
      axis.title = element_text(size = 22),
      axis.text = element_text(size = 22),
      panel.grid.minor = element_blank()
    )
  
  # Add optimal value
  p <- p +
    geom_vline(xintercept = optimal_disp, 
               color = "darkgreen", linetype = "dashed", linewidth = 1) +
    annotate("text", 
             x = optimal_disp, 
             y = max(valid_results$nll, na.rm = TRUE) * 0.95,
             label = paste("Optimal:", round(optimal_disp, 3)),
             color = "darkgreen", size =16, hjust = -0.1)
  
  print(p)
  
  # Save plot
  # ggsave("dispersion_sensitivity_analysis.png", p, 
  #        width = 16, height = 10, dpi = 600)
  
  # Save results
  # write_csv(valid_results, "dispersion_sensitivity_results.csv")
  # cat("Results saved to: dispersion_sensitivity_results.csv\n")
}