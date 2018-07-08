library(plotly)

scat <- data.frame(read.table("dataset3.txt"))
colnames(scat) <- c("u", "v")

ggplot(scat) +
  geom_point(aes(x = u, y = v), color = "gray20", size = 1) +
  ggtitle(paste("Component Means and Covariances \n (Iteration ", t, ")", sep = "")) +
  labs(x = expression(X[1]), y = expression(X[2])) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      margin = margin(b = 20, unit = "pt"),
      lineheight = 1.15,
      size = 16
    ),
    axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
    axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    text = element_text(size = 14)
  ) +
  geom_path(data = EllipsesDf, aes(x=x, y=y, colour=as.character(group)))


df <- data.frame(x = c(1, 2, 3, 2, 3, 4, 3, 3, 3),
                 y = c(5, 6, 7, 4, 5, 6, 5, 6, 7),
                 f = c(1, 1, 1, 2, 2, 2, 3, 3, 3))

p <- df %>% 
  plot_ly (
    x = ~ x,
    y = ~ y,
    frame = ~f,
    type = 'scatter',
    mode = 'lines',
    showlegend = F
  ) %>% 
  add_trace(
    scat,
    x =  ~ V1,
    y =  ~ V2,
    name = 'scat',
    mode = 'markers'
  )
p