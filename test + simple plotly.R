library(shiny)
library(ggplot2)
library(ggthemes)
library(plotly)

ui <- fluidPage(  
  titlePanel("Plotly"),
  sidebarLayout(
    sidebarPanel(),
    mainPanel(
      plotlyOutput("plot2"))))

server <- function(input, output) {
  
  output$plot2 <- renderPlotly({
    print(
      ggplotly(
        ggplot(data = mtcars, aes(x = disp, y = cyl)) + geom_smooth(method = 
                                                                      lm, formula = y~x) + geom_point() + theme_gdocs()))
    
  })
}

shinyApp(ui, server)