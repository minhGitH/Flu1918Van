library(tidyverse)
library(deSolve)

# Mortality dataframe
mortality <- read_csv("Vancouver_influenza_mortality.csv", 
                      show_col_types = FALSE,
                      col_names = TRUE)[(-27),] |> 
  mutate(`Week ending` = dmy(`Week ending`)) 

times <- seq(0,25,1)

df <- data.frame(
  time = times,
  d_inc = mortality$`Number of deaths`
)

# Temperature dataframe
temp_1918 <- read_csv("Vancouver_daily_temp_1918.csv", 
                      show_col_types = FALSE,
                      col_names = TRUE) |> 
  rename(date=`Date/Time`,mean_temp_daily = `Mean Temp (°C)`) 

temp_1918_filtered <- temp_1918 |> 
  select(all_of(names(temp_1918)[c(5,14)])) |> 
  filter(date > as.Date("1918-09-28")) 

temp_1919 <- read_csv("Vancouver_daily_temp_1919.csv", 
                      show_col_types = FALSE,
                      col_names = TRUE) |> 
  rename(date=`Date/Time`,mean_temp_daily = `Mean Temp (°C)`) 

temp_1919_filtered <- temp_1919 |> 
  select(all_of(names(temp_1919)[c(5,14)])) |> 
  filter(date < as.Date("1919-03-30")) 

df_temp_daily <- bind_rows(temp_1918_filtered,temp_1919_filtered)

df_temp_daily <- df_temp_daily |> 
  mutate(time = as.numeric((date - min(date))/7)) |> 
  rename(temp = mean_temp_daily)

# NPI function
f_npi2 <- (function() {
  # Precompute constants
  t_1 <- as.numeric(difftime(as.Date("1918-10-18"), as.Date("1918-09-29"), units = "weeks"))
  t_2   <- as.numeric(difftime(as.Date("1918-11-19"), as.Date("1918-09-29"), units = "weeks"))
  
  t_3 <- as.numeric(difftime(as.Date("1919-01-26"), as.Date("1918-09-29"), units = "weeks"))
  
  function(time, f_1, f_2, dif) {
    val <- rep(1, length(time))  # default value is 1
    
    if (dif>0) {
      # Ramp down: from 1 to f_1
      idx <- time >= t_1 & time < t_1 + dif
      val[idx] <- 1 + (f_1 - 1) * (time[idx] - t_1) / dif
      
      # constant f_1
      idx <- time >= t_1 + dif & time < t_2
      val[idx] <- f_1
      
      # Ramp up: from f_1 to 1
      idx <- time >= t_2 & time < t_2 + dif
      val[idx] <- f_1 + (1 - f_1) * (time[idx] - t_2) / dif
      
      # Ramp down: from 1 to f_2
      idx <- time >= t_3 & time < t_3 + dif
      val[idx] <- 1 + (f_2 - 1) * (time[idx] - t_3) / dif
      
      # constant f_2
      idx <- time >= t_3 + dif
      val[idx] <- f_2  
    }
    
    val
  }
})()

sirs_7b <- function(beta_0, xi, df_temp,  k, lambda_, f_npi, f_1, f_2, dif,
                    gamma, phi, omega, S0, I0, R0, D0, P0, times) {
  # Define the time-dependent beta function
  temp_fit <- loess(temp ~ time, data = df_temp)
  
  f_npi <- f_npi(times, f_1, f_2, dif)
  
  # The differential equations
  sirs_equations <- function(time, variables, parameters) {
    # Replace negative states with 0
    
    with(as.list(c(variables,parameters)), {
      
      temp_val <- predict(temp_fit, newdata = data.frame(time = time))
      
      # fallback in case of NA
      if (is.na(temp_val)) temp_val <- mean(df_temp$temp, na.rm = TRUE)
      temp_factor <- exp(-xi*temp_val)
      #
      N <- S0 + I0 + R0 + D0
      
      I <- exp(logI)
      
      dP <- phi*gamma*I-lambda_*P
      
      idx <- which.min(abs(times - time))
      beta <- beta_0 * temp_factor * f_npi[idx] * (1 - P/N)^(k*1000)
      
      dS <- -beta / N * S * I + omega * R
      dlogI <- beta / N * S - gamma
      dR <- (1 - phi) * gamma * I - omega * R
      dD <- phi * gamma * I
      
      return(list(c(dS, dlogI, dR, dD, dP), beta = beta, d_inc = dD))
    })
  }
  #
  logI0 <- if(I0 > 0) log(I0) else log(.Machine$double.eps)
  initial_values <- c(S = S0, logI = logI0, R = R0, D = D0, P = P0)
  #
  parameters <- list(
    beta_0 = beta_0,
    xi = xi,
    k = k,
    lambda_ = lambda_,
    f_1 = f_1,
    f_2 = f_2,
    dif = dif,
    gamma = gamma,
    phi = phi,
    omega = omega,
    f_npi= f_npi
  )
  #
  ode(initial_values, times, sirs_equations, parameters, rtol = 1e-6, atol = 1e-8)   
}