library(plotly)
library(quantmod)

getSymbols("AAPL",src='yahoo')

df <- data.frame(Date=index(AAPL),coredata(AAPL))
df <- tail(df, 30)
df$ID <- seq.int(nrow(df))

accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}

LLHgraph <- LLHgraph %>%
  accumulate_by(~Iteration)

LLHgraph %>%
  plot_ly(
    x = ~ Iteration,
    y = ~ LogLikelihood,
    frame = ~ frame,
    type = 'scatter',
    mode = 'lines',
    fill = 'tozeroy',
    fillcolor = 'rgba(50, 50, 50, 0.33)',
    line = list(color = 'rgba(30, 30, 30, 0.33)'),
    # text = ~paste("Day: ", ID, "<br>Close: $", AAPL.Close),
    hoverinfo = 'text'
  ) %>%
  layout(
    title = "Log-Likelihood Over Iterations",
    yaxis = list(
      title = "Log-Likelihood",
      zeroline = F
    ),
    xaxis = list(
      showline = F,
      showticklabels = F,
      zeroline = T,
      showgrid = T
    )
  ) %>%
  config(displayModeBar = F) %>%
  animation_opts(frame = 100,
                 transition = 0,
                 redraw = FALSE) %>%
  animation_slider(currentvalue = list(prefix = "Iteration "))

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="cumAnimations-filled-area")
chart_link