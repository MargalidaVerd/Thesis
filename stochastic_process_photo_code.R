library(ggplot2)
library(dplyr)
library(purrr)

t_vals <- 1:7

set.seed(16102001)
dens_sources <- list(
  rnorm(1000, mean = 0, sd = 1),
  rnorm(1000, mean = 2, sd = 0.5),
  rnorm(1000, mean = -1, sd = 1.5),
  rnorm(1000, mean = 1, sd = 0.7),
  rnorm(1000, mean = 0, sd = 0.4),
  rnorm(1000, mean = -2, sd = 1),
  rnorm(1000, mean = 1.5, sd = 0.6)
)

expansion_factor <- 0.25

dens_list <- map(dens_sources, density)

dens_df <- map2_dfr(dens_list, t_vals, function(dens, t) {
  data.frame(
    t = t + dens$y * expansion_factor,
    y = dens$x,
    grupo = paste0("Y", t)
  )
})

mean_df <- map2_dfr(dens_sources, t_vals, function(x, t) {
  data.frame(
    t = t,
    y = mean(x)
  )
})

max_y <- max(dens_df$y - 0.5)

seg_df <- data.frame(
  x    = t_vals,
  xend = t_vals,
  y    = -9,      
  yend =  5
)

ggplot() +
  geom_polygon(data = dens_df, aes(x = t, y = y, group = grupo), 
               fill = "#1753b3", alpha = 0.9) +
  geom_line(data = mean_df, aes(x = t, y = y), color = "black", linewidth = 0.4) +
  geom_point(data = mean_df, aes(x = t, y = y), color = "black", size = 0.4) +
  
  geom_segment(data = seg_df,
               aes(x = x,
                   xend = xend,
                   y  = y,
                   yend = yend),
               colour = "grey40",
               linewidth = 0.2) +
  geom_text(data = data.frame(t = t_vals, y = max_y + 0.1),
            aes(x = t, y = y, label = paste0("Y_", t_vals)),
            vjust = 0, hjust=0.3, size = 3) + 
  scale_x_continuous(
    breaks = t_vals,
    labels = paste0(t_vals),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  labs(x = "Time", y = "Y") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),  
    panel.background = element_rect(fill = "white", colour = NA),
    plot.background = element_rect(fill = "white", colour = NA),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 0),
    axis.title = element_text(size = 14)
  )

