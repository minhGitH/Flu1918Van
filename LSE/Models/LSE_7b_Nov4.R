source("model_framework.R")
library("FME")

# Define your cost function (using globals dif, gamma, omega, phi)
sirs_cost <- function (p,dif) {
  
  beta_0 <- p["beta_0_init"]
  xi <- p["xi_init"]
  k <- p["k_init"]
  lambda_ <- p["lambda_init"]
  f_1 <- p["f_1_init"]
  f_2 <- p["f_2_init"]
  
  gamma = gamma; omega = omega; phi = phi;
  out  <- as.data.frame(sirs_7b(beta_0= beta_0, gamma = gamma, omega = omega, phi = phi,
                    k = k, lambda_ = lambda_, xi = xi, df_temp = df_temp_daily,
                    f_npi = f_npi2, f_1 = f_1, f_2 = f_2, dif = dif,
                    S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))
  
  modCost(model = out, obs = df)
}

library(snow)
library(parallel) 
library(pbapply)

# Fixed parameters for model
gamma <- 1/(5/7)
omega <- 0.05
phi <- 0.0083

# --- Parameter grid --- 

param_grid <- expand.grid(
  beta_0_init = c(5,10,15,20),
  xi_init     = c(0.057, 0.025, 0.1),
  k_init      = c(1,2,2.5),
  lambda_init = c(0.5,1),
  f_1_init   = c(0.3,0.5,0.7,0.9),
  f_2_init   = c(0.4,0.6,0.8,1),
  dif         = c(2/7,5/7,1),
  stringsAsFactors = FALSE
)

# --- Create cluster ---
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores, type = "SOCK")

# --- Export needed variables and libraries to workers ---
clusterExport(cl, varlist = c("sirs_cost", "sirs_7b", "df_temp_daily", "f_npi2",
                              "gamma", "omega", "phi","param_grid",
                              "df", "times"))
clusterEvalQ(cl, {
  library(FME)     # for modFit
  library(tidyverse)
  library(deSolve)
})


# Nelder-Mead method

# --- Function to run one row ---
run_fit_ND <- function(i) {
  
  row <- param_grid[i, ]
  p_init <- row[-length(row)]
  
  # Safe fit
  fit_try <- tryCatch({
    modFit(
      f = function(p) sirs_cost(p, dif = row$dif),
      p = p_init,
      lower = c(0,0,0,0,0,0),
      upper = c(40,1,3,7,1,1),
      method = "Nelder-Mead",
      control = list(maxit = 2000)
    )
  }, error = function(e) {
    message("Fit failed for row ", i, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(fit_try)) return(NULL)
  
  # Extract fitted params
  beta_0_fit <- fit_try$par["beta_0_init"]
  xi_fit <- fit_try$par["xi_init"]
  k_fit      <- fit_try$par["k_init"]
  lambda_fit <- fit_try$par["lambda_init"]
  f_1_fit   <- fit_try$par["f_1_init"]
  f_2_fit   <- fit_try$par["f_2_init"]
  rmse <- sqrt(fit_try$ms)
  
  # Run model with fitted params
  out_fitted <- sirs_7b(
    beta_0 = beta_0_fit, gamma = gamma, omega = omega, phi = phi,
    k = k_fit, lambda_ = lambda_fit, xi = xi_fit, df_temp = df_temp_daily,
    f_1 = f_1_fit, f_2 = f_2_fit, dif = row$dif, f_npi = f_npi2,
    S0 = 100000, I0 = 10, R0 = 0, D0 = 0, P0 = 0, times = times
  )
  
  tibble(
    beta_0_init = row$beta_0_init,
    xi_init     = row$xi_init,
    k_init      = row$k_init,
    lambda_init = row$lambda_init,
    f_1_init   = row$f_1_init,
    f_2_init     = row$f_2_init,
    dif         = row$dif,
    beta_0_fit  = beta_0_fit,
    xi_fit      = xi_fit,
    k_fit       = k_fit,
    lambda_fit  = lambda_fit,
    f_1_fit    = f_1_fit,
    f_2_fit    = f_2_fit,
    rmse        = rmse
  )
}

# --- Run in parallel ---
run_time <- system.time({
  results_list_ND <- pblapply(seq_len(nrow(param_grid)), run_fit_ND, cl = cl)
})

# --- Combine results ---
df_results_ND <- bind_rows(results_list_ND)

# --- Stop cluster ---
stopCluster(cl)

print(run_time)
df_results_ND

# --- Save results ---
write_csv(df_results_ND,"LSE_full_results_Nov9.csv")