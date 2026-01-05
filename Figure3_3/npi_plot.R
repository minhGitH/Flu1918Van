source("model_framework.R")

times <- seq(0, 25, 0.1)
f_1 <- 0.6
f_2 <- 0.8
dif <- 5/7

# Call the vectorized function
values2 <- f_npi2(times, f_1, f_2, dif)

# Quick check of first few results
head(values2)


p <- ggplot(data.frame(time = times, value = values2), aes(x = time, y = value)) +
  geom_line(color = "blue", size = 1.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  labs(x = "Time (weeks)", y = expression(H(t))) +
  theme_bw(base_size = 20) +  # enlarge all text
  theme(
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x  = element_text(size = 20),
    axis.text.y  = element_text(size = 20)
  )

p

# ggsave("npi_plot.png", p, width = 8, height = 6, dpi = 600)