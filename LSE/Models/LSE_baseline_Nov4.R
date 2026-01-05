source("model_framework.R")
library("FME")

# Define your cost function (using globals dif, gamma, omega, phi)
sirs_cost <- function (p,dif) {
  
  beta_0 <- p["beta_0_init"]
  
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
xi <- 0
k <- 0
lambda_ <- 0
f_1 <- 1
f_2 <- 1

# --- Parameter grid --- 

param_grid <- expand.grid(
  # beta_0_init = c(5,10,15,20,25,30,35),
  beta_0_init = c(5),
  dif=c(0),
  stringsAsFactors = FALSE
)

# --- Create cluster ---
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores, type = "SOCK")

# --- Export needed variables and libraries to workers ---
clusterExport(cl, varlist = c("sirs_cost", "sirs_7b", "df_temp_daily", "f_npi2",
                              "gamma", "omega", "phi","param_grid","xi","k",
                              "lambda_","f_1","f_2",
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
      lower = c(0),
      upper = c(40),
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
  rmse <- sqrt(fit_try$ms)
  
  # Run model with fitted params
  out_fitted <- sirs_7b(
    beta_0 = beta_0_fit, gamma = gamma, omega = omega, phi = phi,
    k = k, lambda_ = lambda_, xi = xi, df_temp = df_temp_daily,
    f_1 = f_1, f_2 = f_2, dif = row$dif, f_npi = f_npi2,
    S0 = 100000, I0 = 10, R0 = 0, D0 = 0, P0 = 0, times = times
  )
  
  tibble(
    beta_0_init = row$beta_0_init,
    xi_fit      = xi,
    k_fit       = k,
    lambda_fit  = lambda_,
    f_1_fit    = f_1,
    f_2_fit    = f_2,
    dif         = row$dif,
    beta_0_fit  = beta_0_fit,
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
# write_csv(df_results_ND,"LSE_baseline_results_Nov9.csv")