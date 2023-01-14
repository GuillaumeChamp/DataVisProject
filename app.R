#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

data <- read.csv("data.csv", header = TRUE) 

# Convert to timpestamp
data$timestamp = strptime(data$timestamp, "%Y-%m-%d %H:%M:%OS", tz="UTC")

# Panels ------------------------------------------------------------------

intro_panel <- tabPanel(
  "Introduction",
  numericInput("row", "rows", value = 100),
  column(12,tableOutput("table"))
)

second_panel <- tabPanel(
  "Frequency",
  titlePanel("The frequency map")
)

third_panel <- tabPanel(
  "Popularity",
  titlePanel("The popularity chart"),
  
  sidebarLayout( position = "right",
                 
                 sidebarPanel(
                   helpText("Create a chart that display the number of active 
                            player through the time"),
                   
                   selectInput(
                     "y_var",
                     label = "Select a Y variable:",
                     # temporary just to make the app run
                     choices = c(1,2,3),
                     selected = "1"
                   ),
                   
                   textOutput("text"),
                   verbatimTextOutput("legend"),
                   
                   br(),
                   
                 ),
                 
                 mainPanel()
                 
                 #mainPanel(plotOutput("popularity_chart")) 
  ),
)

# Define UI ---------------------------------------------------------------

ui <- navbarPage(
  strong("Menu"),
  intro_panel,
  second_panel,
  third_panel
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$table <- renderTable(head(MyDat,input$row), options = list(pageLength = 3))
  
#  output$popularity_chart <- renderPlot(
#    ggplot(data=test, aes(x=test$timestamp, y=test$user_id, group=1)) +
#      geom_line()+ scale_x_date(date_labels = "%Y-%m-%d %H:%M:%OS") +
#      geom_point()
# )
}

# Run the application 
shinyApp(ui = ui, server = server)
