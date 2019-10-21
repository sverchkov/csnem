# Licensed under the BSD 3-Clause License
# Copyright (c) 2019, Yuriy Sverchkov

# Shiny app to run CSNEM from a GUI.

library(shiny)
library(plotly)
library(ggplot2)
library(ggnetwork)
library(network)
library(sna)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("Context-Specific Nested Effects Model"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
         fileInput("in_file", "Input data file"), #TODO: set accept:
         #, accept = c("text/csv", "text/comma-separated-values", "text/plain", ".csv"))
         selectInput("data_type", "Input data type", "log-posterior-odds")
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("netPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$netPlot <- renderPlot({
    # Placeholder dummy net
    n <- network(rgraph(10, tprob=0.2), directed=TRUE)
    n %v% "family" <- sample(letters[1:3], 10, replace=TRUE)
    n %v% "importance" <- sample(1:3, 10, replace=TRUE)
    e <- network.edgecount(n)
    set.edge.attribute(n, "type", sample(letters[24:26], e, replace=TRUE))
    set.edge.attribute(n, "day", sample(1:3, e, replace=TRUE))

    net <- ggnetwork(n)

    # Actual plot render
    ggplot(net, aes(x = x, y = y, xend = xend, yend = yend)) +
      geom_nodes(aes(color="black", size=importance)) +
      geom_nodelabel_repel(aes(color = family, label = LETTERS[vertex.names]), fontface = "bold") +
      geom_edges(aes(linetype = type), color = "grey50") +
      theme_blank()
  })
}

# Run the application
shinyApp(ui = ui, server = server)

