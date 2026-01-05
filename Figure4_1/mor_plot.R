source("model_framework.R")


# Mortality fig

m <- ggplot(mortality, aes(x = `Week ending`, y = `Number of deaths`)) + 
  geom_line() + 
  geom_point() +
  labs(x = "Week ending", y = "Number of deaths") +
  theme_bw(base_size = 28) +
  theme(
    axis.text = element_text(size = 28),
    axis.title = element_text(size = 32, face = "bold")
  )
m
# ggsave("mortality.png", m, width = 16, height = 9, dpi = 600)