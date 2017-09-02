data(iris)
require(shiny)

sepal.length <-
  sapply(unique(iris$Species), function(s) {
    iris[which(iris$Species == s), "Sepal.Length"]
  })
data <-
  data.frame(
    "mean" = apply(sepal.length, 2, mean),
    "sd" = apply(sepal.length, 2, sd),
    "species" = unique(iris$Species)
  )

server <- shinyServer(function(input, output, session) {
  
  # Save plot in reactive
  plot <- reactiveValues(main = NULL, layer1 = NULL)
  plot$main <- ggplot(data = data, aes(x = species, y = mean, fill = species)) +
    geom_bar(stat = "identity") 
  
  observe({
    # print("render")
    output$plot <- renderPlot({
      plot$main + plot$layer1
    })
  })
  
  observeEvent(input$add_bars, {
    # Calculate standard deviation
    plot$layer1 <- geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd))
  })
  
})

ui <- shinyUI(fluidPage(
  plotOutput("plot"),
  actionButton("add_bars","Add error bars")
))

shinyApp(ui = ui, server = server)  
