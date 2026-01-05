source("model_framework.R")

# Nelder_Mead failed due to named numeric vector????

library(lme4)

nll = function(p, dif) {
  # beta_0 <- p["beta_0_init"]
  # xi <- p["xi_init"]
  # k <- p["k_init"]
  # lambda_ <- p["lambda_init"]
  # f_1 <- p["f_1_init"]
  # f_2 <- p["f_2_init"]
  
  beta_0 <- p[1]
  k <- p[2]
  lambda_ <- p[3]
  
  out  <- as.data.frame(sirs_7b(beta_0= beta_0, gamma = gamma, omega = omega, phi = phi,
                                k = k, lambda_ = lambda_, xi = xi, df_temp = df_temp_daily,
                                f_npi = f_npi2, f_1 = f_1, f_2 = f_2, dif = dif,
                                S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = times))
  
  return(-sum(dnbinom(df$d_inc,size = 1/1.07, mu = out$d_inc,log=TRUE)))
}

library(snow)
library(parallel) 
library(pbapply)

# Fixed parameters for model
gamma <- 1/(5/7)
omega <- 0.05
phi <- 0.0083
xi <- 0
f_1 <- 1
f_2 <- 1


# --- Parameter grid --- 
param_grid <- expand.grid(
  beta_0_init = c(5,10,15,20,25,30,35),
  k_init      = c(1,2,2.5),
  lambda_init = c(0.5,1),
  dif = c(0),
  stringsAsFactors = FALSE
)


# --- Create cluster ---
num_cores <- detectCores() 
cl <- makeCluster(num_cores, type = "SOCK")

# --- Export needed variables and libraries to workers ---
clusterExport(cl, varlist = c("nll", "sirs_7b", "df_temp_daily", "f_npi2",
                              "gamma", "omega", "phi","xi","f_1","f_2",
                              "param_grid", "df", "times"))
clusterEvalQ(cl, {
  library(tidyverse)
  library(deSolve)
  library(lme4)
})


# Nelder-Mead method
# --- Function to run one row ---
run_fit_ND <- function(i) {
  
  row <- param_grid[i, ]
  p <- as.numeric(row[-length(row)])
  
  # Safe fit
  fit_try <- tryCatch({
    Nelder_Mead(
      fn = function(p) nll(p, dif = row$dif),
      par = p,
      lower = c(0,0,0),
      upper = c(40,3,7),
      control = list(maxfun = 2000)
    )
  }, error = function(e) {
    message("Fit failed for row ", i, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(fit_try)) return(NULL)
  
  # Extract fitted params
  beta_0_fit <- fit_try$par[1]
  k_fit <- fit_try$par[2]
  lambda_fit <- fit_try$par[3]
  
  nlogll <- (fit_try$fval) #negative loglikelihood
  
  # Run model with fitted params
  out_fitted <- sirs_7b(
    beta_0 = beta_0_fit, gamma = gamma, omega = omega, phi = phi,
    k = k_fit, lambda_ = lambda_fit, xi = xi, df_temp = df_temp_daily,
    f_1 = f_1, f_2 = f_2, dif = row$dif, f_npi = f_npi2,
    S0 = 100000, I0 = 10, R0 = 0, D0 = 0, P0 = 0, times = times
  )
  
  tibble(
    beta_0_init = row$beta_0_init,
    k_init           =  row$k_init,
    lambda_init      = row$lambda_init,
    dif         = row$dif,
    xi_fit      = xi,
    f_1_fit    = f_1,
    f_2_fit    = f_2,
    beta_0_fit  = beta_0_fit,
    k_fit  = k_fit,
    lambda_fit  = lambda_fit,
    nlogll       = nlogll
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
write_csv(df_results_ND,"MLE_response_results_Nov21.csv")
