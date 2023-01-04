#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

MyDat <- read.csv("data/header.txt", header = TRUE) 
# Define UI for application that draws a histogram
ui <- fluidPage(
  numericInput("row", "rows", value = 100),
  column(12,tableOutput("table"))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- renderTable(head(MyDat,input$row), options = list(pageLength = 3))
}

# Run the application 
shinyApp(ui = ui, server = server)
