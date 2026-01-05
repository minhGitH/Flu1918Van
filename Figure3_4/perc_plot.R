source("model_framework.R")
library(cowplot)

# Fixed variables
gamma <- 1/(5/7)
omega <- 0.05
phi <- 0.025

out_full <- sirs_7b(beta_0= 17.62, gamma = gamma, omega = omega, phi = phi,
                    k = 0.46, lambda_ = 0.065, xi = 0.13,df_temp = df_temp_daily, 
                    f_npi = f_npi2, f_1 = 0.69,f_2 = 1, dif = 2/7,
                    S0 = 100000,  I0 = 10, R0 = 0, D0=0, P0 =0, times = seq(0,25,0.01))

# plot(out_full, obs = df, which = c("beta.1", "d_inc"), las = 1, 
#      obspar = list(pch = 16, col = "red"))

df_perc <- tibble(times=seq(0,25,0.01), value = out_full[,6])

# 100010 is the total population N = S+I+R+D

# k*1000 because it was standardized by dividing by 1000

fit_perc <- (1-out_full[,6]/100010)^(0.46*1000)

df_response <- tibble(times=seq(0,25,0.01), value = fit_perc)


p1 <- ggplot(df_perc, aes(x = times, y = value)) +
  geom_line(linewidth = 1, color = "red") +
  theme_minimal(base_size = 30) +   
  theme(
    axis.title.x = element_text(size = 30), 
    axis.title.y = element_text(size = 30),  
    axis.text.x  = element_text(size = 30),  
    axis.text.y  = element_text(size = 30)   
  ) +
  labs(
    x = "Time (weeks)",
    y = expression(P(t))
  )

p2 <- ggplot(df_response, aes(x = times, y = value)) +
  geom_line(linewidth = 1, color = "red") +
  theme_minimal(base_size = 30) +   
  theme(
    axis.title.x = element_text(size = 30),  
    axis.title.y = element_text(size = 30),  
    axis.text.x  = element_text(size = 30),  
    axis.text.y  = element_text(size = 30)   
  ) +
  labs(
    x = "Time (weeks)",
    y = expression((1 - P(t)/N)^k)
  )

# Create the combined plot with larger labels
human_response_plot <- plot_grid(
  p1, p2,
  labels = c("A", "B"),
  label_size = 30,                
  ncol = 2,
  align = "hv",
  label_fontface = "bold"
)

human_response_plot

# ggsave("Human_response_plot.png", human_response_plot,
#        width = 16, height = 9, dpi = 600)